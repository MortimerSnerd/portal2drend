## Simple vector font used for debugging without having to haul
## around a font texture.
import 
  comalg, geom, glsupport, opengl, strformat, strutils, verts

const
  EndLine = int8.low
    ## Lines are sequence of coordinates to draw a line between.  EndLines 
    ## is a sentinel that marks the end of a continuous line.

  EndLetter = EndLine + 1
    ## End sequence of lines for a letter.

  LetterHtScaleX1* = 10  
   ## Some letters (Q, ',') have a declination below 10

  UnknownChar = '\1'
    ## Code for character we draw when we encounter a char not in the font.

var strokes: seq[int8]  ## Coordinate pairs, or EndLines, or EndLetter.
var letters: seq[(char, int32)] ## Lookup for index of letters into the strokes seq.

proc letter(c: char; lines: openarray[int8]) = 
  ## Defines the strokes for a letter.  Should be called in ascending order, 
  ## so the `letter` seq is ordered.  Don't include the EndLetter sentinel
  ## in `lines`.
  add(letters, (c, int32(len(strokes))))
  add(strokes, lines)
  add(strokes, EndLetter)

proc Init*() = 
  ## Initializes the letters.  Should be called before any other module function.
  letter(UnknownChar, [1.int8, 6,  0, 7,  0, 9,  1, 10,  5, 10,  6, 9,  6, 6,  5, 5,  4, 5, 3, 4, 3, 2,  EndLine, 3, 0,  3, 1])
  letter('!', [1.int8, 0,  0, 7,  EndLine, 0, 9, 0, 10])
  letter(',', [1.int8, 9,  1, 10,  0, 11])
  letter('.', [0.int8, 9, 0, 10])
  letter('0', [0.int8, 1,  0, 9,  1, 10,  3, 10,  4, 9,  4, 1,  3, 0,  1, 0, 0, 1,  EndLine,  2, 4,  2, 6])
  letter('1', [0.int8, 1,  2, 0,  2, 10])
  letter('2', [0.int8, 1,  1, 0,  3, 0,  4, 1,  4, 5,  0, 10,  4, 10])
  letter('3', [0.int8, 1,  1, 0,  3, 0,  4, 1,  4, 4,  3, 5,  1, 5,  EndLine,  3, 5,  4, 6,  4, 9,  3, 10,  1, 10,  0, 9])
  letter('4', [0.int8, 0,  0, 4,  4, 4,  EndLine,  3, 0,  3, 10])
  letter('5', [0.int8, 9,  1, 10,  3, 10,  4, 9,  4, 5,  3, 4,  1, 4,  0, 4,  0, 0,  4, 0])
  letter('6', [4.int8, 1,  3, 0,  1, 0,  0, 1,  0, 9,  1, 10,  3, 10,  4, 9,  4, 5,  3, 5,  0, 5])
  letter('7', [0.int8, 0,  4, 0, 2, 10])
  letter('8', [0.int8, 1,  0, 4,  1, 5, 0, 6,  0, 9,  1, 10,  3, 10,  4, 9,  4, 6,  3, 5,  4, 4,  4, 1,  3, 0,  1, 0,  0, 1,  EndLine,  1, 5,  3, 5])
  letter('9', [4.int8, 10,  4, 1,  3, 0,  1, 0,  0, 1,  0, 4,  1, 5,  4, 5])
  letter(':', [0.int8, 2,  0, 3,  EndLine,  0, 7,  0, 8])
  letter('?', [0.int8, 3,  0, 1,  1, 0,  3, 0,  4, 1,  4, 5,  3, 6,  3, 7,  EndLine,  3, 9, 3, 10])
  letter('A', [0.int8, 10,  0, 3,  3, 0,  6, 3,  6, 10,  EndLine,  0, 5,  6, 5])
  letter('B', [0.int8, 0,  0, 10,  5, 10,  6, 8,  6, 5,  5, 4,  6, 3,  6, 2,  4, 0,  0, 0,  EndLine, 0, 4,  5, 4])
  letter('C', [6.int8, 1,  5, 0,  1, 0,  0, 1, 0, 9,  1, 10,  5, 10,  6, 9])
  letter('D', [0.int8, 0,  0, 10,  3, 10,  6, 8,  6, 2,  3, 0,  0, 0])
  letter('E', [6.int8, 0,  0, 0,  0, 10,  6, 10,  EndLine,  0, 5,  5, 5])
  letter('F', [6.int8, 0,  0, 0,  0, 10,  EndLine,  0, 5,  5, 5])
  letter('G', [6.int8, 1,  5, 0,  1, 0,  0, 1,  0, 9,  1, 10,  5, 10,  6, 9,  6, 6,  4, 6])
  letter('H', [0.int8, 0,  0, 10,  EndLine,  6, 0,  6, 10,  EndLine,  0, 5,  6, 5])
  letter('I', [0.int8, 0,  6, 0,  EndLine,  0, 10,  6, 10,  EndLine,  3, 0,  3, 10])
  letter('J', [0.int8, 7,  0, 8,  2, 10,  5, 10,  6, 7,  6, 0,  4, 0])
  letter('K', [0.int8, 0,  0, 10,  EndLine,  6, 0,  0, 5,  6, 10])
  letter('L', [0.int8, 0,  0, 10,  6, 10])
  letter('M', [0.int8, 10,  1, 0,  3, 5,  5, 0,  6, 10])
  letter('N', [0.int8, 10,  0, 0,  6, 10,  6, 0])
  letter('O', [0.int8, 2,  0, 8,  2, 10,  4, 10,  6, 8,  6, 2,  4, 0,  2, 0,  0, 2])
  letter('P', [0.int8, 10,  0, 0,  4, 0,  6, 1,  6, 4,  5, 5,  0, 5])
  letter('Q', [0.int8, 2,  0, 8,  2, 10,  4, 10,  6, 8,  6, 2,  4, 0,  2, 0,  0, 2, EndLine,  6, 11,  4, 8])
  letter('R', [0.int8, 10,  0, 0,  5, 0,  6, 1,  6, 4,  5, 5,  0, 5,  EndLine, 4, 5,  6, 10])
  letter('S', [0.int8, 9,  1, 10,  5, 10,  6, 9,  6, 6,  5, 5,  1, 5,  0, 4,  0, 1,  1, 0,  5, 0,  6, 1])
  letter('T', [0.int8, 0,  6, 0,  EndLine,  3, 0,  3, 10])
  letter('U', [0.int8, 0,  0, 9,  1, 10,  5, 10,  6, 9,  6, 0])
  letter('V', [0.int8, 0,  3, 10,  6, 0])
  letter('W', [0.int8, 0,  1, 10,  3, 5,  5, 10,  5, 0])
  letter('X', [0.int8, 0,  6, 10,  EndLine,  6, 0,  0, 10])
  letter('Y', [3.int8, 10,  3, 5,  0, 0,  EndLine, 3, 5,  6, 0])
  letter('Z', [0.int8, 0,  6, 0,  0, 10,  6, 10])

  checkArray(letters)

iterator lines(loc: int; scale: float32 = 1.0f) : (V2f, V2f) = 
  var pc = loc
  var last = (-1.0f, -1.0f)

  while pc < len(strokes) and strokes[pc] != EndLetter:
    if strokes[pc] == EndLine:
      last = (-1.0f, -1.0f)
      inc(pc)
    else:
      let x = strokes[pc]; inc(pc)
      let y = strokes[pc]; inc(pc)
       
      assert x >= 0 and y >= 0, &"Bad letter def @ {loc}"
      let p = (float32(x) * scale, float32(y) * scale)
      if last[0] >= 0:
        yield (last, p)
      last = p

proc findLetter(c: char) : int = 
  ## Index into strokes for a given letter, or -1 if not found.
  # We have no lowercase letters, so map to upper for now.
  result = linear(letters, toUpperAscii(c), -1)
  if result < 0:
    # We have no lowercase, so map to upper.
    result = linear(letters, UnknownChar)

proc renderLetter(batch: VertBatch[VtxColor,uint16];  c: char; topLeft: V2f; scale: float32 = 1.0f; color: V4f = WhiteG) : V2f = 
  ## Renders `c` at `topLeft` with the given `color`.  Returns the max extent of the letter.
  let loc = findLetter(c)

  if loc >= 0:
    for (p1, p2) in lines(loc, scale):
      result.x = max(result.x, max(p1.x, p2.x))
      result.y = max(result.y, max(p1.y, p2.y))
      AddLine(batch, VtxColor(pos: p1 + topLeft, color: color), VtxColor(pos: p2 + topLeft, color: color))

proc Text*(batch: VertBatch[VtxColor,uint16]; msg: string; topLeft: V2f; scale: float32 = 1.0f; color: V4f = WhiteG) : V2f {.discardable.} = 
  ## Renders the string at the given location. Does not wrap text. 
  ## Optionally returns the extent of the text rendered.
  const LetterSpace = 2.0f
  var pos = topLeft

  for c in msg:
    if c == ' ':
      pos.x += LetterSpace*2*scale
    else:
      let ext = renderLetter(batch, c, pos, scale, color)
      pos.x += ext.x + LetterSpace * scale

  result = (pos.x, LetterHtScaleX1 * scale)

when isMainModule:
  import sdl2, zstats
  proc go() = 
    const 
      WW = 1024
      WH = 640

    Init()
    var allRes: ResourceSet

    assert sdl2.init(INIT_VIDEO) == SdlSuccess, $sdl2.getError()

    let window = glsupport.Init(allRes) do () -> WindowPtr:
      createWindow("VFONT Example", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, WW, WH, SDL_WINDOW_OPENGL)

    let uniBlk = NewBufferObject(allRes)
    var uni: StdUniforms

    uni.mvp = orthoProjectionYDown[float32](0, WW, 0, WH, -10.0f, 10.0f)
    uni.tint = WhiteG
    glBindBufferBase(GL_UNIFORM_BUFFER, StdUniformsBinding, uniBlk.handle)
    Populate(uniBlk, GL_UNIFORM_BUFFER, uni.addr, GL_DYNAMIC_DRAW)

    let colorShader = NewProgram(allRes, 
                                 [NewShaderFromFile(allRes, "color.vtx", GL_VERTEX_SHADER), 
                                 NewShaderFromFile(allRes, "color.frag", GL_FRAGMENT_SHADER)])
    Use(colorShader)

    let verti = NewBufferObject(allRes)
    let vertv = NewBufferObject(allRes)
    let verts = NewVertBatch[VtxColor,uint16](GL_LINES)

    BindAndConfigureArray(vertv, VtxColorDesc)
    glLineWidth(1.0f)
    glViewport(0, 0, WW, WH)

    var running = true
    while running:
      let start = getTicks()
      var ev = Event(kind: QuitEvent)

      while pollEvent(ev):
        if ev.kind == KeyUp:
          running = false

      Clear(verts)
      glClearColor(0.0f, 0.0f, 0.0f, 0.0f)
      glClear(GL_COLOR_BUFFER_BIT)

      Text(verts, "ABCDEFGHIJKLMNOPQRSTUVWXYZ, 0123456789. WOT?", (20.0f, 20.0f), 2.0f, BlueG)
      Text(verts, "Now: your face asplode!! %%%", (20.0f, 100.0f), 3.0f, WhiteG)

      SubmitAndDraw(verts, vertv, verti, GL_LINES)
      SwapWindow(window)

      let now = getTicks()
      zstats.Record(znFrameTime, float(now - start) * 0.001)

  try:
    go()
    echo GC_getStatistics()
    zstats.PrintReport()
  except:
    echo "UNCAUGHT EXCEPTION"
    var e = getCurrentException()
    while not e.isNil:
      echo(&"{e.msg}\n{e.getStackTrace()}")
      echo "---------"
      e = e.parent
