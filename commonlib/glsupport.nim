## As a rule, objects that refer to GL handles will be ref objects, 
## so we don't get multiple copies of the handle references that 
## won't see changes, like recreated resources when the context is lost.
##
import
    geom, opengl, math, platform, sdl2, sdl2/image, strformat, zstats

type
    ResourceKind = enum
      rkBuffer, rkTexture, rkShader, rkProgram, rkFrameBuffer, rkVertexArray

    ResourceSet* = object
      # A set of OpenGL resources that aren't managed by the GC, and can be
      # freed all at once.  Will free everything in the reverse order they were added
      # when the object is destructed. Shouldn't be passed by value.  (when newRuntime 
      # rolls around, we could control how copies work, if we want to support them).
      arr: seq[tuple[kind: ResourceKind; handle: GLuint]]

proc `=destroy`*(rs: var ResourceSet) = 
  ## Deletes all resources in the reverse order to which they were added.
  for i in countdown(len(rs.arr)-1, 0):
    case rs.arr[i].kind
    of rkBuffer:
      glDeleteBuffers(1, rs.arr[i].handle.addr)
    of rkTexture:
      glDeleteTextures(GLsizei(1), rs.arr[i].handle.addr)
    of rkShader:
      glDeleteShader(rs.arr[i].handle)
    of rkProgram:
      glDeleteProgram(rs.arr[i].handle)
    of rkFrameBuffer:
      glDeleteFrameBuffers(1, rs.arr[i].handle.addr)
    of rkVertexArray:
      glDeleteVertexArrays(1, rs.arr[i].handle.addr)

  setLen(rs.arr, 0)

proc `=sink`*(rs: var ResourceSet; s: ResourceSet) = 
  raise newException(ObjectAssignmentError, "Don't copy ResourceSets")

proc Add*(rs: var ResourceSet; kind: ResourceKind; handle: GLuint|GLint) = 
  add(rs.arr, (kind: kind, handle: GLuint(handle)))

# Mark all functions with gl calls with the EGL effect.
type
  EGL* = object of RootEffect 
    ## OpenGL effect tag for functions.

  StdUniforms* = object
    ## Uniforms common to most shaders.  It's ok for
    ## this to contain uniforms that not all shaders
    ## need. The idea is to manage just a couple of uniform 
    ## types, to avoid dicking around with uniform locations 
    ## all over the place.
    ##
    ## This must follow the std140 data layout.
    mvp*: Matrix4x4f
    tint*: V4f
    cameraWorldPos*: V4f

let
    GLSLUniformBlock* = """#version 410
    layout(std140) uniform StdUniforms {
       mat4 mvp;
       vec4 tint;
       vec4 cameraWorldPos;
    };"""
      ## For the love of god and all that's holy, this had
      ## better match the StdUniforms declaration above.
      ## This gets prepended to shaders automatically, so
      ## we don't have to do some templating system for the shader
      ## text to avoid repeating the uniform blocks all over the place.


{.pragma: gl,tags: [EGL].}

var
  textureBytesUploadedPerFrame* : int
  otherBytesUploadedPerFrame : int
    ## Per frame accumulator for stBytesUploadedPerFrame.  
#
# Module initializaation.
#
proc Init*(rset: var ResourceSet; winCreate: proc () : WindowPtr) : WindowPtr {.gl.} = 
  ## Call to initialize GL support once you're ready to create your
  ## SDL2 window.
  zstats.InitForThread()
  discard glSetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE)
  discard glSetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3)
  discard glSetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 2)
  discard glSetAttribute(SDL_GL_RED_SIZE, 8)
  discard glSetAttribute(SDL_GL_GREEN_SIZE, 8)
  discard glSetAttribute(SDL_GL_BLUE_SIZE, 8)
  discard glSetAttribute(SDL_GL_ALPHA_SIZE, 8)
  discard glSetAttribute(SDL_GL_DOUBLEBUFFER, 1)
  when defined(srgb):
    const SDL_GL_FRAMEBUFFER_SRGB_CAPABLE = 23
    discard glSetAttribute(cast[GLattr](SDL_GL_FRAMEBUFFER_SRGB_CAPABLE), 1)
  discard glSetAttribute(SDL_GL_DEPTH_SIZE, 16)
  discard glSetAttribute(SDL_GL_STENCIL_SIZE, 8)

  result = winCreate()
  if result != nil:
    let ctx = glCreateContext(result)
    if ctx.isNil:
      raise newException(GLerror, "Failed creating gl context: " & $getError())

    loadExtensions()

    # The code assumes this is enabled.
    enableAutoGLErrorCheck(true)

    #TODO The following is dumb.  Asking for a 3.2 context now, and
    #     you're supposed to have a VAO active whenever calling
    #     glVertexAttribPointer.  So we bind a vao once here on startup.
    #     Need to look to see if we need to manage this more smartly 
    #     in the future.
    var vao : GLuint
    glGenVertexArrays(1, vao.addr)
    Add(rset, rkVertexArray, vao)
    glBindVertexArray(vao)

    when defined(srgb):
      glEnable(GL_FRAMEBUFFER_SRGB)

#
# Buffer objects
#
type
  BufferObject* = distinct GLuint

proc NewBufferObject*(rset: var ResourceSet) : BufferObject =
  glGenBuffers(1, cast[ptr GLuint](result.addr))
  Add(rset, rkBuffer, GLuint(result))

proc Populate*[T](bo: BufferObject; 
                  target: GLenum;
                  data: var openarray[T];
                  usage: GLenum = GL_STATIC_DRAW) {.gl.} = 
    ## Allocates the buffer and initializes it with the data from 'data'.
    if len(data) > 0:
      glBindBuffer(target, bo.GLuint)
      let bsz = sizeof(T)*len(data)
      otherBytesUploadedPerFrame += bsz
      glBufferData(target, GLsizeiptr(bsz), 
                   addr(data[0]), usage)

proc Populate*[T](bo: BufferObject; 
                  target: GLenum; 
                  data: ptr T, 
                  usage: GLenum = GL_STATIC_DRAW) {.gl.} = 
  ## Allocates the buffer if necessary, and initializes it with 'data'.
  glBindBuffer(target, bo.GLuint)
  otherBytesUploadedPerFrame += sizeof(T)
  glBufferData(target, GLsizeiptr(sizeof(T)), 
               data, usage)

proc Upload*(uni: var StdUniforms; blk: BufferObject) = 
  ## Uploads the entire uniforms block to ``blk``.
  Populate(blk, GL_UNIFORM_BUFFER, uni.addr, GL_DYNAMIC_DRAW)

#
# Textures
#
type
  Texture* = ref object
    iHandle: GLuint
    width*, height*: int
    target: GLenum
    fmt, ifmt, dataType: GLenum

  TextureParams* = object
    minFilter*: GLint
    magFilter*: GLint
    wrapS*: GLint
    wrapT*: GLint

     
proc ApplyParameters*(t: Texture; p: TextureParams) {.gl.} = 
  ## Sets a group of parameters on a texture.  Helpful, as
  ## I don't think you can have two textures reference the same
  ## buffer memory, but with different parameters, so these need to
  ## be changed at times.
  glBindTexture(t.target, t.iHandle)
  if p.minFilter != 0:
    glTexParameteri(t.target, GL_TEXTURE_MIN_FILTER, p.minFilter)

  if p.magFilter != 0:
    glTexParameteri(t.target, GL_TEXTURE_MAG_FILTER, p.magFilter)

  if p.wrapS != 0:
    glTexParameteri(t.target, GL_TEXTURE_WRAP_S, p.wrapS)

  if p.wrapT != 0:
    glTexParameteri(t.target, GL_TEXTURE_WRAP_T, p.wrapT)

proc ToTexCoord*[T](tx: Texture; p: V2X[T]) : V2f = 
  ## Converts pixel coordinates into a texture into texture coordinates.
  result = (x: (p.x / tx.width.T).float32, 
            y: (p.y / tx.height.T).float32)

proc MaybeResize*(tx: Texture; dims: V2i) = 
  ## Resizes the texture ``tx`` if its current dims are != ``dims``.
  if tx.iHandle != 0 and dims != (tx.width, tx.height):
    glBindTexture(tx.target, tx.iHandle)
    glTexImage2D(GL_TEXTURE_2D, GLint(0), GLint(tx.fmt), 
                 GLsizei(dims.x), GLsizei(dims.y), GLint(0), 
                 tx.ifmt, tx.dataType, nil)
    tx.width = dims.x
    tx.height = dims.y

proc resetPixelStoreVars() = 
  ## Make sure no-one has left some funny state in the pixel store
  ## vars before uploading pixel data.
  glPixelStorei(GL_UNPACK_ROW_LENGTH, 0)
  glPixelStorei(GL_UNPACK_SKIP_ROWS, 0)
  glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0)
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1)

proc MkTexture*(w, h: int) : Texture = 
  ## Create a texture with no data or format associated with it.
  assert w > 0
  assert h > 0
  new(result)
  glGenTextures(1.GLsizei, result.iHandle.addr)
  result.width = w
  result.height = h
  result.target = GL_TEXTURE_2D

proc MkTexture(data: ptr byte; w, h: int;  format = GL_RGBA; internFormat = GL_RGBA; dataType = GL_UNSIGNED_BYTE) : Texture {.gl.} = 
  ## Helper function for loadTexture functions.
  result = MkTexture(w, h)
  result.fmt = format
  result.ifmt = internFormat
  result.dataType = dataType

  # We don't care if 'data' is nil, because that has a valid meaning - 
  # glTexImage2D just allocates the memory for the texture and assumes
  # you'll upload the data later.
  glBindTexture(GL_TEXTURE_2D, result.iHandle)
  resetPixelStoreVars()
  textureBytesUploadedPerFrame += w * h * 4  # Assuming RGBA8
  glTexImage2D(GL_TEXTURE_2D, GLint(0), GLint(format), 
               GLsizei(w), GLsizei(h), GLint(0), 
               internFormat, dataType, data)

proc internalFormatFor(s: SurfacePtr) : GLenum = 
    ## What's the right internalFormat parameter for glTextImage*
    ## Only support a couple of modes right now.
    case s.format.BytesPerPixel
    of 3:
        result = GL_RGB
    of 4:
        result = GL_RGBA
    else:
        raise newException(ValueError, "Could not determine format: " & repr(s.format))

proc formatFor(s: SurfacePtr; internFormat: GLenum) : GLenum = 
    # Whats the right pixel format for glTexImage2D?
    case s.format.Rmask.int64
    of 0x000000FF:
        # RGBA, little endian.
        if internFormat == GL_RGBA:
            when defined(srgb):
              result = GL_SRGB_ALPHA
            else:
              result = GL_RGBA
        else:
          when defined(srgb):
            result = GL_SRGB
          else:
            result = GL_RGB

    of 0x00FF0000:
        if internFormat == GL_RGBA:
            result = GL_BGRA
        else:
            result = GL_BGR
    else:
        raise newException(ValueError, "Unrecognized pixel format, red mask = " & $s.format.Rmask)

proc bpp(format: GLenum) : int =
    case format
    of GL_RGBA:
        result = 4
    of GL_RGB:
        result = 3
    else:
        raise newException(ValueError, "Unimplemented format: " & $format.int)

proc LoadTexture*(data: ptr byte; w, h: int; format: GLenum = GL_RGBA; internFormat = GL_RGBA; dataType = GL_UNSIGNED_BYTE) : Texture {.gl.} = 
  result = MkTexture(data, w, h, format)
  result.fmt = format
  result.ifmt = internFormat
  result.dataType = dataType
  resetPixelStoreVars()
  textureBytesUploadedPerFrame += w * h * 4  # Assuming RGBA8
  glTexImage2D(GL_TEXTURE_2D, GLint(0), GLint(format), 
               GLsizei(w), GLsizei(h), GLint(0), 
               internFormat, dataType, data)

proc LoadTexture*(data: var openarray[byte]; w, h: int; format: GLenum = GL_RGBA; internFormat = GL_RGBA;
                  dataType = GL_UNSIGNED_BYTE) : Texture {.gl.} = 
    let calcBytes = w * h * format.bpp

    if calcBytes != len(data):
        raise newException(ValueError, "Expecting $1 bytes, but array has $2 bytes" % [$calcBytes, $len(data)])

    result = MkTexture(addr(data[0]), w, h, format)
    result.fmt = format
    result.ifmt = internFormat
    result.dataType = dataType
    resetPixelStoreVars()
    textureBytesUploadedPerFrame += w * h * 4  # Assuming RGBA8
    glTexImage2D(GL_TEXTURE_2D, GLint(0), GLint(format), 
                 GLsizei(w), GLsizei(h), GLint(0), 
                 internFormat, dataType, addr(data[0]))

proc PremultAlpha*(surf: SurfacePtr) = 
  ## Our blending assumes premultiplied alpha.  
  ## Textures are already premultiplied by the atlas
  ## builder, but this may need to be called for 
  ## dynamically generated images.
  #NOTE Images are in sRGB, so technically we're 
  #      multiplying linear Alpha against non-linear
  #      color channels.  Not technically correct, but
  #      not addressed because it seems to be good enough.
  if surf.format.BytesPerPixel != 4 or surf.format.AMask == 0:
    return

  var 
    r, g, b, a: uint8  
    pxp : ptr uint32 = cast[ptr uint32](surf.pixels)

  template cmult(al: uint8; cl: uint8) : untyped = 
    uint8((al.int * cl.int) div 255)

  for y in 0..<surf.h.int:
    let row = pxp
    for x in 0..<surf.w.int:
      getRGBA(pxp[], surf.format, r, g, b, a)
      r = cmult(a, r)
      g = cmult(a, g)
      b = cmult(a, b)
      pxp[] = mapRGBA(surf.format, r, g, b, a)
      pxp = cast[ptr uint32](cast[int](pxp) + 4)
    pxp = cast[ptr uint32](cast[int](row) + surf.pitch.int)

proc LoadTexture(tx: Texture; fileName: string; createMipmap: bool = false; multAlpha: bool = false) {.gl.} = 
    ## Loads and initializes an existing texture empty texture.  
    ## loading the next texture. Raises IOError if there's a problem loading the file.
    assert tx.iHandle == 0
    let surf = load(platform_data_path(fileName))

    if surf.isNil:
       raise newException(IOError, "Could not load texture " & fileName & ": " & $getError())

    if multAlpha:
      PremultAlpha(surf)

    let iformat = internalFormatFor(surf)
    let format = formatFor(surf, iformat)

    tx.width = int(surf.w)
    tx.height = int(surf.h)
    tx.target = GL_TEXTURE_2D
    tx.fmt = format
    tx.ifmt = iformat
    tx.dataType = GL_UNSIGNED_BYTE

    try:
        glGenTextures(GLsizei(1), addr(tx.iHandle))
        glBindTexture(GL_TEXTURE_2D, tx.iHandle)
        resetPixelStoreVars()
        textureBytesUploadedPerFrame += surf.w.int * surf.h.int * 4  # Assuming RGBA8
        glTexImage2D(GL_TEXTURE_2D, GLint(0), GLint(iformat), 
                     GLsizei(surf.w), GLsizei(surf.h), 
                     GLint(0), format, 
                     GL_UNSIGNED_BYTE, surf.pixels)

        if createMipmap:
          glGenerateMipMap(GL_TEXTURE_2D)

        ## Resource name.
        #tx.name = fileName

    finally:
        freeSurface(surf)

proc LoadTexture*(rs: var ResourceSet; fileName: string; createMipMap: bool = false; multAlpha: bool = false) : Texture {.gl.} = 
    ## Allocates a new texture, and loads the specified image into it.
    new(result)
    LoadTexture(result, fileName, createMipMap, multAlpha)
    Add(rs, rkTexture, result.iHandle)

#
# Shaders
#
type
  # A single shader.
  Shader* = distinct GLuint

  # A Shader program, made of multiple shaders.
  Program* = distinct GLuint

template getCompileLog(logInfoFn: untyped; s: untyped) : untyped = 
  const MaxMsg = 512
  let msg = cast[cstring](alloc(MaxMsg))
  var msgLen : GLsizei = 0

  try:
      logInfoFn(s.GLuint, GLsizei(MaxMsg), addr(msgLen), msg)
      if msgLen > 0:
        $msg
      else:
        ""
  finally:
      dealloc(msg)

proc NewShader(origin: string; src: string; shaderType: GLenum) : Shader {.gl.} = 
    ## Creates a shader from 'src' and compiles it.  
    ## 'origin' is where the source came from, and is only
    ## used for error reporting.
    result = Shader(glCreateShader(shaderType))
    let ca = allocCStringArray([src])
    try:
        glShaderSource(result.GLuint, GLsizei(1), ca, nil)
    finally:
        deallocCStringArray(ca)

    try:
        var status : GLint
        glCompileShader(result.GLuint)
        glGetShaderiv(result.GLuint, GL_COMPILE_STATUS, addr(status))
        if status == 0:
            raise newException(GLerror,"")

        when defined(debug):
          # Print out any shader compiler output to catch warnings.
          let msg = getCompileLog(glGetShaderInfoLog, result)

          if len(msg) > 0:
            echo "COMPILATION " & origin
            echo msg
            echo ""

    except GLerror:
      let msg = getCompileLog(glGetShaderInfoLog, result)

      if len(msg) > 0:
        raise newException(GLError, origin & ": " & msg)
      else:
        raise newException(GLError, "Problem compiling shader " & origin & ", but could not get error message")

proc NewShaderFromFile*(rset: var ResourceSet; relPath: string, shaderType: GLenum) : Shader = 
  ## Loads a shader from a file.
  let absPath = platform_data_path(relPath)
  let src     = readFile(absPath)

  result = NewShader(absPath, GLSLUniformBlock & src, shaderType)
  Add(rset, rkShader, result.GLuint)

#
# Vertex array attribute descriptions.
#
type
  AttributeIndex* = enum
    ## Attribute types for vertex array descriptions.  
    ## These are bound to attribute locations when a shader program is
    ## created.
    PosAttrib, TexCoordAttrib, ColorAttrib, NormalAttrib

  VertexAttrib* = object
    ## Description of a vertex attrib for a vertex attrib array.
    index*: AttributeIndex  
    numComponents*: GLint
    kind*: GLenum          ## GL_FLOAT, ...
    stride*: GLsizei       ## #bytes between consecutive entries, or 
                           ##  0 for tightly packed array
    initialOffset*: int    ## Byte offset of first entry from start of buffer.

type
  FrameBuffer* = ref object
    ## For rendering to a texture.
    handle*: GLuint
    txt*: Texture
    depthTxt: Texture
    dims*: V2i

proc NewFrameBuffer*(rs: var ResourceSet; dims: V2i; format, internFormat: GLenum; hasDepthOrStencil: bool = true) : FrameBuffer = 
  let fb = FrameBuffer(dims: dims)
  glGenFramebuffers(1, addr(fb.handle))
  glBindFramebuffer(GL_FRAMEBUFFER, fb.handle)
  Add(rs, rkFrameBuffer, fb.handle)

  fb.txt = MkTexture(nil, dims.x, dims.y, format, internFormat)
  Add(rs, rkTexture, fb.txt.iHandle)
  ApplyParameters(fb.txt, TextureParams(minFilter: GL_NEAREST, magFilter: GL_NEAREST))
  glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, fb.txt.iHandle, 0)

  if hasDepthOrStencil:
    fb.depthTxt = MkTexture(nil, dims.x, dims.y, GL_DEPTH24_STENCIL8, GL_DEPTH_STENCIL, GL_UNSIGNED_INT_24_8)
    Add(rs, rkTexture, fb.depthTxt.iHandle)
    glBindTexture(GL_TEXTURE_2D, fb.depthTxt.iHandle)
    ApplyParameters(fb.depthTxt, TextureParams(minFilter: GL_NEAREST, magFilter: GL_NEAREST))
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_TEXTURE_2D, fb.depthTxt.iHandle, 0)

  var arr = [GL_COLOR_ATTACHMENT0]
  glDrawBuffers(cint(len(arr)), addr(arr[0]))
  assert glCheckFramebufferStatus(GL_FRAMEBUFFER) == GL_FRAMEBUFFER_COMPLETE
  glBindFramebuffer(GL_FRAMEBUFFER, 0)

  return fb

proc MaybeResize*(fb: FrameBuffer; dims: V2i) = 
  ## Resizes the textures attached to a framebuffer, if they are a different
  ## size than ``dims``.
  MaybeResize(fb.txt, dims)
  MaybeResize(fb.depthTxt, dims)




const
    ## Uniform buffer binding for StdUniforms block.  Specify this in the
    ## layout of the uniform blocks in the shader.  ie:
    ##    layout(binding = 1, std140) uniform StdUniforms {...}
    StdUniformsBinding* = 1

    ## Masks and GL format we should use so SDL surfaces and OpenGL textures
    ## are compatible.
    DispAMask*      = 0xFF000000
    DispBMask*      = 0x00FF0000
    DispGMask*      = 0x0000FF00
    DispRMask*      = 0x000000FF
    DispDepth*      = 32
    DispGLFormatIntern*   = GL_RGBA
    SDLPixelFormat* = SDL_PIXELFORMAT_ABGR8888

when defined(srgb):
  const DispGLFormat*   = GL_SRGB_ALPHA
else:
  const DispGLFormat*   = GL_RGBA


proc NewProgram*(rset: var ResourceSet; shaders: openarray[Shader]) : Program = 
  ## Creates a new program by linking the given shaders together.
  result = Program(glCreateProgram())
  Add(rset, rkProgram, result.GLuint)
  for i in 0..<len(shaders):
    try:
      glAttachShader(result.GLUint, shaders[i].GLuint)
    except GLerror:
      raise newException(GLerror, "For shader parameter " & $i & ": " & $getError(), 
                         parentException = getCurrentException())
  # Bind standard attrib locations
  glBindAttribLocation(result.GLuint, PosAttrib.GLuint, cstring("position"))
  glBindAttribLocation(result.GLuint, TexCoordAttrib.GLuint, cstring("texcoord"))
  glBindAttribLocation(result.GLuint, ColorAttrib.GLuint, cstring("color"))
  glBindAttribLocation(result.GLuint, NormalAttrib.GLuint, cstring("normal"))

  glLinkProgram(result.GLuint)
  when defined(debug):
    let msg = getCompileLog(glGetProgramInfoLog, result.GLuint)

    if len(msg) > 0:
      raise newException(GLError, &"{msg}")

  # For linux, doesn't quite support the binding attribute for layout()
  # in shaders, so set the binding manually here.
  let bi = glGetUniformBlockIndex(result.GLuint, "StdUniforms")
  if bi.GLenum == GL_INVALID_INDEX:
    raise newException(GLError, "Could not find uniform block index")
  glUniformBlockBinding(result.GLuint, bi, StdUniformsBinding)

proc Use*(p: Program) {.gl.} = 
  ## Uses the program
  glUseProgram(p.GLuint)

#
# Handle access and other exposed internals.
#
type
  GLValueResource = BufferObject|Program|Shader
  GLRefResource = Texture|FrameBuffer

proc handle*(b: GLRefResource) : GLuint = 
  ## Read access for the underlying OpenGL handle for GL resources.
  if b.isNil:
    result = 0
  else:
    result = b.iHandle

template handle*(b: GLValueResource) : GLuint = b.GLuint

proc SwapWindow*(win: WindowPtr) = 
  ## Call this when finished with a frame to 
  ## swap in the buffer that was just drawn.  We hook it here
  ## to also take care of some per-frame stats we collect.
  glSwapWindow(win)
  zstats.Record(znTextureBytesUploadedPerFrame, textureBytesUploadedPerFrame.float)
  zstats.Record(znOtherBytesUploadedPerFrame, otherBytesUploadedPerFrame.float)
  textureBytesUploadedPerFrame = 0
  otherBytesUploadedPerFrame = 0

proc BindAndConfigureArray*(v: BufferObject; attribs: openarray[VertexAttrib]) {.gl.} = 
    glBindBuffer(GL_ARRAY_BUFFER, v.GLuint)
    for i in 0 .. high(attribs):
        glEnableVertexAttribArray(attribs[i].index.GLuint)
        glVertexAttribPointer(attribs[i].index.GLuint, 
                              attribs[i].numComponents,
                              attribs[i].kind, 
                              GLboolean(GL_FALSE), 
                              attribs[i].stride, 
                              cast[pointer](attribs[i].initialOffset))


proc CreateCompatSurface*(w, h: SomeInteger) : SurfacePtr = 
  ## Creates a SDL surface compatible with the surface spec
  ## defined above.
  assert w >= 0
  assert h >= 0
  createRGBSurface(0, w.cint, h.cint, DispDepth.cint, DispRMask.uint32, 
                   DispGMask.uint32, DispBMask.uint32, DispAMask.uint32)

#
# colors, in SDL and GL formats.
#
let
  White* = (r: 255'u8, g: 255'u8, b: 255'u8, a: 255'u8)
  Black* = (r: 0'u8, g: 0'u8, b: 0'u8, a: 255'u8)
  Green*  = (r: 0'u8, g: 255'u8, b: 0'u8, a: 255'u8)
  Blue*  = (r: 0'u8, g: 0'u8, b: 255'u8, a: 255'u8)
  Red*   = (r: 255'u8, g: 0'u8, b: 0'u8, a: 255'u8)
  WhiteG* = (1.0f, 1.0f, 1.0f, 1.0f)
  BlackG* = (0.0f, 0.0f, 0.0f, 1.0f)
  GreenG*  = (0.0f, 1.0f, 0.0f, 1.0f)
  BlueG*  = (0.0f, 0.0f, 1.0f, 1.0f)
  RedG*   = (1.0f, 0.0f, 0.0f, 1.0f)

proc glColor*(c: sdl2.Color) : V4f = 
  ## Convert sdl color to V4f format recognized by OpenGL APIs
  (c.r.float32 / 255.0f, c.g.float32/255.0f, c.b.float32/255.0f, c.a.float32/255.0f)

#
# Common vertex formats and the descriptions used to bind them.
#
type 
    TxVtx* = object
      ## Vertex with position and texture coordinates.
      pos*: V3f
      tc*:  V2f

    TxVtxColor* = object
      ## Vertex with position, texture, and a tint color.
      pos*: V3f
      tc*: V2f
      color*: V4f # Technially, a tint that's applied with the texture colors.

    TxVtxNorm* = object
      ## Vertex with position, texcoord and normal.
      pos*: V3f
      tc*: V2f
      normal*: V3f

    VtxColor* = object
      ## Vertex with color.
      pos*: V2f
      color*: V4f

    VtxColorNorm* = object
      pos*: V3f
      color*: V4f
      norm*: V3f

let
    VtxColorDesc* = @[VertexAttrib(index: PosAttrib, 
                                   numComponents: 2, 
                                   kind: cGL_FLOAT, 
                                   stride: sizeof(VtxColor).GLsizei, 
                                   initialOffset: 0), 
                      VertexAttrib(index: ColorAttrib, 
                                   numComponents: 4, 
                                   kind: cGL_FLOAT, 
                                   stride: sizeof(VtxColor).GLsizei, 
                                   initialOffset: 4*2)]
      ## Desc used to bind the attributes for arrays of VtxColor objects.

    VtxColorNormDesc* = @[VertexAttrib(index: PosAttrib, 
                                   numComponents: 3, 
                                   kind: cGL_FLOAT, 
                                   stride: sizeof(VtxColorNorm).GLsizei, 
                                   initialOffset: 0), 
                      VertexAttrib(index: ColorAttrib, 
                                   numComponents: 4, 
                                   kind: cGL_FLOAT, 
                                   stride: sizeof(VtxColorNorm).GLsizei, 
                                   initialOffset: 4*3), 
                      VertexAttrib(index: NormalAttrib, 
                                   numComponents: 3, 
                                   kind: cGL_FLOAT, 
                                   stride: sizeof(VtxColorNorm).GLsizei, 
                                   initialOffset: 4*7)]
      ## Desc used to bind the attributes for arrays of VtxColorNorm objects.

    TxVtxDesc* = @[VertexAttrib(index: PosAttrib, 
                                numComponents: 3, 
                                kind: cGL_FLOAT, 
                                stride: sizeof(TxVtx).GLsizei, 
                                initialOffset: 0), 
                   VertexAttrib(index: TexCoordAttrib, 
                                numComponents: 2, 
                                kind: cGL_FLOAT, 
                                stride: sizeof(TxVtx).GLsizei, 
                                initialOffset: 4*3)]
      ## Desc used to bind attributes for arrays of TxVtx objects.

    TxVtxColorDesc* = @[VertexAttrib(index: PosAttrib, 
                                     numComponents: 3, 
                                     kind: cGL_FLOAT, 
                                     stride: sizeof(TxVtxColor).GLsizei, 
                                     initialOffset: 0), 
                        VertexAttrib(index: TexCoordAttrib, 
                                     numComponents: 2, 
                                     kind: cGL_FLOAT, 
                                     stride: sizeof(TxVtxColor).GLsizei, 
                                     initialOffset: 4*3), 
                        VertexAttrib(index: ColorAttrib, 
                                     numComponents: 4, 
                                     kind: cGL_FLOAT, 
                                     stride: sizeof(TxVtxColor).GLsizei, 
                                     initialOffset: 4*5)]
      ## Desc used to bind attributes for arrays of TxVtx objects.

    TxVtxNormDesc* = @[VertexAttrib(index: PosAttrib, 
                                    numComponents: 3, 
                                    kind: cGL_FLOAT, 
                                    stride: sizeof(TxVtxNorm).GLsizei, 
                                    initialOffset: 0), 
                       VertexAttrib(index: TexCoordAttrib, 
                                    numComponents: 2, 
                                    kind: cGL_FLOAT, 
                                    stride: sizeof(TxVtxNorm).GLsizei, 
                                    initialOffset: 4*3), 
                       VertexAttrib(index: NormalAttrib, 
                                    numComponents: 3,
                                    kind: cGL_FLOAT, 
                                    stride: sizeof(TxVtxNorm).GLsizei, 
                                    initialOffset: 4 * 3 + 4 * 2)]


