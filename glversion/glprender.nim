import 
  collections/deques, geom, glsupport, math, opengl, os, parseutils, sdl2, sdl2/image, 
  random, strformat, zstats, verts, vfont

const
  WW = int32(1024)
  WH = int32(780)
  EyeHeight = 6.0f
  DuckHeight = 2.5f
  HeadMargin = 1
  KneeHeight = 2
  hfov = 0.73f*float32(WH)
  vfov = 0.2f*float32(WH)

# Color constants, int32 ARGB pixel.  A is not used by the renderer.
const
  CeilingColor = 0x222222
  CeilingTopBorder = 0x111111
  CeilingBottomBorder = CeilingTopBorder

  FloorColor = 0x0000aa
  FloorTopBorder = 0x0000ff
  FloorBottomBorder = FloorTopBorder

  WallTopBorder = 0x000000
  WallBottomBorder = 0x000000
  WallBaseColor = 0x010101     # Base color that is multipled by inverse of distance from player.

  # Bottom wall, wall below a sector when it is higher than our sector.
  BottomWallTopBorder = 0x000000
  BottomWallBottomBorder = 0x000000
  BottomWallBaseColor = 0x000700   # Base color that multiplied by inverse of distance from player.

template glColor(c: int32) : V4f = 
  (float32((c shr 16) and 255) / 255, 
   float32((c shr 8) and 255) / 255,
   float32(c and 255) / 255, 
   1.0f)

type
  Sector = object
    floor, ceil: float32
    vertex: seq[V2f]
    neighbors: seq[int8]
    npoints: uint16

  Player = object
    where, velocity: V3f 
    angle, anglesin, anglecos, yaw: float32
    sector: uint16

  DrawModes = enum
    FullSpeed, SectorAtATime

var NumSectors : Natural
var surface: SurfacePtr
var window: WindowPtr
var sectors: seq[Sector]
var player: Player
var drawMode = FullSpeed

template clamp(a, mi, ma: untyped) : untyped = min(max(a, mi), ma)
template vxs(x0, y0, x1, y1: untyped) : untyped = x0*y1 - x1*y0
template Overlap(a0,a1,b0,b1: untyped) : untyped = min(a0, a1) <= max(b0, b1) and min(b0, b1) <= max(a0, a1)
# IntersectBox: Determine whether two 2D-boxes intersec
template IntersectBox(x0, y0, x1, y1, x2, y2, x3, y3: untyped) : untyped = Overlap(x0, x1, x2, x3) and Overlap(y0, y1, y2, y3)
# PointSide: Determine which side of a line the point is on. Return value: <0, =0 or >0
template PointSide(px, py, x0, y0, x1, y1: untyped) : untyped = vxs(x1-x0, y1 - y0, px - x0, py - y0)
# Intersect: Calculate the point of intersection between two lines.
template Intersect(x1, y1, x2, y2, x3, y3, x4, y4: untyped) : untyped = 
  (x: vxs(vxs(x1,y1, x2,y2), x1-x2, vxs(x3,y3, x4,y4), x3-x4) / vxs(x1-x2, y1-y2, x3-x4, y3-y4), 
   y: vxs(vxs(x1,y1, x2,y2), y1-y2, vxs(x3,y3, x4,y4), y3-y4) / vxs(x1-x2, y1-y2, x3-x4, y3-y4))

proc parsedFloat(line: var string; ix: var int; val: var float32) : bool = 
  ix += skipWhitespace(line, ix)
  var v: float
  let np = parseFloat(line, v, ix)
  if np == 0:
    false
  else:
    ix += np
    val = float32(v)
    true

proc parsedUint16(line: var string; ix: var int; val: var uint16) : bool = 
  ix += skipWhitespace(line, ix)
  var v: int
  let np = parseInt(line, v, ix)
  if np == 0:
    false
  else:
    ix += np
    val = uint16(v)
    true

proc parsedIntList(line: var string; ix: var int; res: var seq[int]) : bool = 
  result = true
  var s: string
  while ix < len(line):
    ix += skipWhitespace(line, ix)
    let np = parseUntil(line, s, {' ', '\t'}, ix)
    if np == 0:
      break

    ix += np

    if s == "#":
      break

    if s == "x":
      add(res, -1)
    else:
      var num: int
      let np = parseInt(s, num, 0)
      if np == 0:
        result = false
        break
      else:
        add(res, num)

  result = result and len(res) > 0

proc LoadData(fn: string) = 
  let fp = open(fn, fmRead)
  try:
    var line, ident: string
    var verts: seq[V2f]
    var lineNo = 0

    while readLine(fp, line):
      lineNo += 1
      if len(line) > 0:
        var ix = 0
       
        template eol() : untyped = ix >= len(line) 
        ix += skipWhitespace(line, ix)
        if not eol() and line[ix] == '#':
          continue

        ix += parseIdent(line, ident, ix)
        if ix > 0:
          if ident == "vertex":
            var fx, fy: float32
            ix += skipWhitespace(line, ix)
            if parsedFloat(line, ix, fy):
              while parsedFloat(line, ix, fx):
                add(verts, (x: fx, y: fy))
                ix += skipWhitespace(line, ix)

          elif ident == "sector":
            var sect: Sector
            var num: seq[int]
            
            if parsedFloat(line, ix, sect.floor) and parsedFloat(line, ix, sect.ceil) and parsedIntList(line, ix, num):
              echo &"num={num} for {lineNo}"
              let np = len(num) div 2
              sect.npoints = uint16(np)

              setLen(sect.vertex, 1) # Reserve one so the vertices can be looped.
              for i in 0..<np:
                add(sect.vertex, verts[num[i]])
              sect.vertex[0] = sect.vertex[^1]

              for i in np..<len(num):
                add(sect.neighbors, int8(num[i]))

              add(sectors, sect)
            else:
              raise newException(IOError, &"Bad sector at {lineNo}")

          elif ident == "player":
            if parsedFloat(line, ix, player.where.x) and parsedFloat(line, ix, player.where.y) and parsedFloat(line, ix, player.angle) and
              parsedUint16(line, ix, player.sector):
              player.where.z = sectors[player.sector].floor + EyeHeight

          else:
            raise newException(IOError, &"Unknown type {ident} on {lineNo}")

    echo &"sectors={sectors}"
    echo &"player={player}"

  finally:
    close(fp)

proc vline(x, py1, py2, top, middle, bottom: int32) = 
  let pix = cast[int](surface.pixels)
  template setp(y, px:  untyped) : untyped = cast[ptr int32](pix + y*WW*4 + x*4)[] = px
  let y1 = clamp(py1, 0, WH - 1)
  let y2 = clamp(py2, 0, WH - 1)
  if y2 == y1:
    setp(y1, middle)
  elif y2 > y1:
    setp(y1, top)
    for y in y1+1..<y2:
      setp(y, middle)
    setp(y2, bottom)
    

const MaxQueue = 32
type 
    Item = object 
      sectorno, sx1, sx2: int32 

    FixedQueue = object
      arr: array[MaxQueue, Item]
      head, tail:  int32
      
var DrawQ: FixedQueue

template enqueue(it: Item) : untyped = 
  DrawQ.arr[DrawQ.head] = it
  DrawQ.head += 1
  if DrawQ.head == MaxQueue: DrawQ.head = 0

template dequeue() : ptr Item = 
  assert DrawQ.head != DrawQ.tail
  let it = DrawQ.arr[DrawQ.tail].addr
  DrawQ.tail += 1
  if DrawQ.tail == MaxQueue: DrawQ.tail = 0
  it

template queueFull() : bool = 
  ((DrawQ.head+MaxQueue+1-DrawQ.tail) mod MaxQueue) == 0

template queueEmpty() : bool = DrawQ.head == DrawQ.tail

var ytop, ybottom: array[WW,int32]
var renderedsectors: seq[int32] 
var todraw: seq[uint16]
  ## Vars persisted over multiple calls to DrawScreen when
  ## drawing one sector at a time.

type
  GLState = ref object
    uni: StdUniforms
    uniblk: BufferObject
    verts, indices: BufferObject
    batch2: VertBatch[VtxColor,uint16]
    batch3: VertBatch[VtxColorNorm,uint16]
    shSolidColor, shSolidColor3: Program

proc NewGLState(rs: var ResourceSet) : GLState = 
  let colorf = NewShaderFromFile(rs, "color.frag", GL_FRAGMENT_SHADER)

  result = GLState(uniblk: NewBufferObject(rs), 
                   verts: NewBufferObject(rs), 
                   indices: NewBufferObject(rs), 
                   batch2: NewVertBatch[VtxColor,uint16](), 
                   batch3: NewVertBatch[VtxColorNorm,uint16](),
                   shSolidColor: NewProgram(rs, 
                                            [NewShaderFromFile(rs, "color.vtx", GL_VERTEX_SHADER), 
                                             colorf]), 
                   shSolidColor3: NewProgram(rs, 
                                             [NewShaderFromFile(rs, "color3.vtx", GL_VERTEX_SHADER),
                                              NewShaderFromFile(rs, "color3.frag", GL_FRAGMENT_SHADER)]))

  glBindBufferBase(GL_UNIFORM_BUFFER, StdUniformsBinding, result.uniblk.handle)
  glEnable(GL_CULL_FACE)
  glDepthFunc(GL_LESS)


type 
  SectorViz = object
    ## Record that's queued for visibility testing a sector.
    sector: uint16 ## Sector to consider for visibility
    lp, rp: V2f 
      ## points in clockwise order than define a field of view starting from
      ## the viewer's current position. A point is in the FOV if it is to the right
      ## of VP -> lp and to the left of VP -> rp

var FOVy = 70.0f
var glvizq = initDeque[SectorViz](64)
  ## Queue of sectors that need to be tested for visibility.

proc DrawScreenGL(gls: GLState; justOneSector: bool) = 
  template SubmitUniforms() : untyped = Populate(gls.uniblk, GL_UNIFORM_BUFFER, gls.uni.addr, GL_DYNAMIC_DRAW)

  glClearColor(0.0f, 0.0f, 0.0f, 1.0f)
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)
  glEnable(GL_DEPTH_TEST)
  glDepthMask(true)

  # Reminder - world coordinates are XY is the ground plane, and Z+ is up.
  # We need to transform to camera coordinates of x+ right, y+ up, and z- fwd.
  # This is modifying the near plane distance to get the FOV, and adjusting
  # the near plane width to keep the correct aspect ratio for the window size.
  let near = 1.0f
  let fvt = tan(FOVy.degrees/2)
  let nphwidth = fvt * (WW.float32/WH.float32) * near
  let nphheight = fvt * near
  let yaw = player.yaw / 4.0f
  let rot = rotation3d((0.0f, 0.0f, 1.0f), player.angle + float32(PI)) * rotation3d((0.0f, -1.0f, 0.0f), yaw)
  let fwd = vec3(rot*(1.0f, 0.0f, 0.0f, 0.0f))
  let eye = player.where
  let mv = lookAt(eye, eye + fwd, (0.0f, 0.0f, -1.0f))
  let p = perspectiveProjectionInf(-nphwidth, nphwidth, -nphheight, nphheight, near, 500.0f)
  let where2 = vec2(player.where)

  gls.uni.mvp = p * mv
  gls.uni.cameraWorldPos = vec4(eye, 1.0f)
  SubmitUniforms()

  Use(gls.shSolidColor3)
  Clear(gls.batch3)
  BindAndConfigureArray(gls.verts, VtxColorNormDesc)

  # First pass - just render everything to see if we can get the basic rendering 
  # correct, without any complications from visibility testing.  Keep the general
  # order of rendering so it tends to go from near to far. Not attempting to make
  # the lines that show where the sector and wall edges are.
  if len(glvizq) == 0:
    setLen(renderedsectors, len(sectors))
    setLen(todraw, 0)
    zeroMem(renderedsectors[0].addr, sizeof(renderedsectors[0]) * len(renderedsectors))
    # Begin whole-screen rendering from where the player is, with
    # the player's FOV.
    let ha = FOVy * 0.5f
    let fwd2 = normalized(vec2(rot*vec4(fwd, 0.0f)))
    addLast(glvizq, SectorViz(sector: player.sector, 
                              lp: rotateAroundOrigin(fwd2, -ha),
                              rp: rotateAroundOrigin(fwd2, ha)))

  # Populate `todraw` by walking through the sectors that are visible, using
  # and propagating the FOV triangle to only walk to sectors that could be visible.
  while len(glvizq) > 0:
    let sv = popFirst(glvizq)

    # Only add to draw list once.  Even once a sector is marked to 
    # be rendered, we still may need to revisit it later with a different
    # FOV that may open the path to yet unvisited neighbors.
    if renderedsectors[sv.sector] == 0:
      add(todraw, sv.sector)
      renderedsectors[sv.sector] = 1

    let sect = sectors[sv.sector].addr
    for s in 0..<int(sect.npoints):
      let neighbor = sect.neighbors[s]

      if neighbor >= 0:
        # A sector edge we can see through.
        let sline = (start: sect.vertex[s+0], extent: sect.vertex[s+1] - sect.vertex[s+0])
        var fvl, fvr: Line2X[float32]

        # We have to look at what side of the sector line we are on 
        # to order the FOV lines consistently.
        if where2.rightOf(sline):
          fvl = (start: where2, extent: sect.vertex[s+0] - where2)
          fvr = (start: where2, extent: sect.vertex[s+1] - where2)
        else:
          fvl = (start: where2, extent: sect.vertex[s+1] - where2)
          fvr = (start: where2, extent: sect.vertex[s+0] - where2)

        let ir1 = calcParametricLineIntersection(fvl, sline)
        let ir2 = calcParametricLineIntersection(fvr, sline)

        if ir1.found and ir1.s2 < ir2.s2 and ir1.s2 <= 1.0f and ir2.s2 >= 0.0f:
          # The sector line is in our FOV, so propagate the FOV
          # through to the neighboring sector.
          let thruPL = sline.start + sline.extent * clamp(ir1.s2)
          let thruPR = sline.start + sline.extent * clamp(ir2.s2)

          addLast(glvizq, SectorViz(sector: uint16(neighbor), lp: thruPL, rp: thruPR))

  # All visible sectors have been recorded, in a rough closest to the player to furthest order.
  # Now just render them.
  var floorVtx, ceilVtx: seq[VtxColorNorm]
  for sn in todraw:
    setLen(floorVtx, 0)
    setLen(ceilVtx, 0)

    let sect = sectors[sn].addr
    for s in 0..<int(sect.npoints):
      add(floorVtx, VtxColorNorm(pos: vec3(sect.vertex[s], sect.floor), 
                                 color: glColor(FloorColor), 
                                 norm: (0.0f, 0.0f, 1.0f)))
      add(ceilVtx, VtxColorNorm(pos: vec3(sect.vertex[s], sect.ceil), 
                                color: glColor(CeilingColor), 
                                norm: (0.0f, 0.0f, -1.0f)))

      let neighbor = sect.neighbors[s]
      let ldir = normalized(sect.vertex[s+1] - sect.vertex[s+0])
      let nn = vec3(ldir.y, ldir.x, 0.0f)

      if neighbor >= 0:
        # Neighboring sector.
        if not queueFull():
          enqueue(Item(sectorno: neighbor))
        discard  
        if sect.ceil > sectors[neighbor].ceil:
          const wcol = glColor(WallBaseColor) * 255.0f
          let nceil = sectors[neighbor].ceil
          let ldir = normalized(sect.vertex[s+1] - sect.vertex[s+0])
          let nn = vec3(ldir.y, ldir.x, 0.0f)
          Triangulate(gls.batch3, [
            VtxColorNorm(pos: vec3(sect.vertex[s+0], nceil), color: wcol, norm: nn), 
            VtxColorNorm(pos: vec3(sect.vertex[s+0], sect.ceil), color: wcol, norm: nn), 
            VtxColorNorm(pos: vec3(sect.vertex[s+1], sect.ceil), color: wcol, norm: nn), 
            VtxColorNorm(pos: vec3(sect.vertex[s+1], nceil), color: wcol, norm: nn)])

        let nfloor = sectors[neighbor].floor
        if sect.floor < nfloor:
          const wcol = glColor(BottomWallBaseColor) * 31.0f
          Triangulate(gls.batch3, [
            VtxColorNorm(pos: vec3(sect.vertex[s+0], sect.floor), color: wcol, norm: nn), 
            VtxColorNorm(pos: vec3(sect.vertex[s+0], nfloor), color: wcol, norm: nn), 
            VtxColorNorm(pos: vec3(sect.vertex[s+1], nfloor), color: wcol, norm: nn), 
            VtxColorNorm(pos: vec3(sect.vertex[s+1], sect.floor), color: wcol, norm: nn)])

      else:
        # Wall on this edge
        let f1 = vec3(sect.vertex[s+0], sect.floor)
        let f2 = vec3(sect.vertex[s+1], sect.floor)
        let c1 = vec3(sect.vertex[s+0], sect.ceil)
        let c2 = vec3(sect.vertex[s+1], sect.ceil)
        const wcol = glColor(WallBaseColor*255)
        Triangulate(gls.batch3, [
          VtxColorNorm(pos: f1, color: wcol, norm: nn),
          VtxColorNorm(pos: c1, color: wcol, norm: nn),
          VtxColorNorm(pos: c2, color: wcol, norm: nn),
          VtxColorNorm(pos: f2, color: wcol, norm: nn)])
        discard  

    Triangulate(gls.batch3, floorVtx)
    TriangulateRev(gls.batch3, ceilVtx)

  SubmitAndDraw(gls.batch3, gls.verts, gls.indices, GL_TRIANGLES)

  # Write OpenGL in the bottom left of the screen.
  gls.uni.mvp = orthoProjectionYDown[float32](0.0f, WW.float32, 0.0f, WH.float32, -10.0f, 10.0f)
  gls.uni.tint = WhiteG
  SubmitUniforms()

  Use(gls.shSolidColor)
  BindAndConfigureArray(gls.verts, VtxColorDesc)
  Clear(gls.batch2)
  glDisable(GL_DEPTH_TEST)
  Text(gls.batch2, &"OpenGL FOVy {FOVy:3.1f}, near {near:1.2f}", (5.0f, WH.float32 - LetterHtScaleX1 - 5.0f), 1.0f, WhiteG)
  Text(gls.batch2, &"ang={player.angle:2.1f}, yaw={player.yaw:2.1f}, fwd={fwd}", (5.0f, WH.float32 - 2 * (LetterHtScaleX1 + 5)))
  SubmitAndDraw(gls.batch2, gls.verts, gls.indices, GL_LINES)

  SwapWindow(window)

{.push overflowChecks: off.}
proc DrawScreen(justOneSector = false) = 
  if queueEmpty():
    for x in 0..<WW: ybottom[x] = WH-1
    zeroMem(ytop[0].addr, sizeof(ytop[0])*WW)
    setLen(renderedsectors, len(sectors))
    zeroMem(renderedsectors[0].addr, sizeof(renderedsectors[0]) * len(renderedsectors))
    # Begin whole-screen rendering from where the player is.
    enqueue(Item(sectorno: int32(player.sector), sx1: 0, sx2: WW-1))

  while not queueEmpty():
    let now = dequeue()

    if (renderedsectors[now.sectorno] and 0x21) != 0: 
      continue
    renderedsectors[now.sectorno] += 1
    
    let sect = sectors[now.sectorno].addr
    # Render each wall of this seector that is facing towards player.
    for s in 0..<int(sect.npoints):
      # Acquire the x,y coordinates of the two endpoints (vertices) of this edge of the sector 
      let vx1 = sect.vertex[s+0].x - player.where.x
      let vy1 = sect.vertex[s+0].y - player.where.y
      let vx2 = sect.vertex[s+1].x - player.where.x
      let vy2 = sect.vertex[s+1].y - player.where.y

      # Rotate them around the player's view 
      let (pcos, psin) = (player.anglecos, player.anglesin)
      var tx1 = vx1*psin - vy1*pcos 
      var tz1 = vx1*pcos + vy1 * psin
      var tx2 = vx2*psin - vy2*pcos 
      var tz2 = vx2*pcos + vy2 * psin

      # Is the wall at least partially in front of the player? 
      if tz1 <= 0 and tz2 <= 0: 
        if justOneSector:
          echo &"Reject sector {now.sectorno} BEHIND"
        continue

      # If it's partially behind the player, clip it against player's view frustrum 
      if tz1 <= 0 or tz2 <= 0:
        let nearz = 1e-4f
        let farz = 5.0f
        let nearside = 1e-5f
        let farsize = 20.0f
        let i1 = Intersect(tx1, tz1, tx2, tz2, -nearside, nearz, -farsize, farz)
        let i2 = Intersect(tx1, tz1, tx2, tz2, nearside, nearz, farsize, farz)

        if tz1 < nearz:
          if i1.y > 0:
            tx1 = i1.x
            tz1 = i1.y
          else:
            tx1 = i2.x
            tz1 = i2.y

        if tz2 < nearz:
          if i1.y > 0.0f:
            tx2 = i1.x
            tz2 = i1.y
          else:
            tx2 = i2.x
            tz2 = i2.y

      # Do perspective transformation 
      let xscale1 = hfov / tz1
      let yscale1 = vfov / tz1
      let x1 = WW div 2 - int32(tx1 * xscale1)
      let xscale2 = hfov / tz2
      let yscale2 = vfov / tz2
      let x2 = WW div 2 - int32(tx2 * xscale2)
      if x1 >= x2 or x2 < now.sx1 or x1 > now.sx2:
        if justOneSector:
          echo &"Reject sector {now.sectorno} X REJECT"
        continue # only render if it's visible

      # Acquire the floor and ceiling heights, relative to where the player's view is 
      let yceil = sect.ceil - player.where.z
      let yfloor = sect.floor - player.where.z

      # Check the edge type. neighbor=-1 means wall, other=boundary between two sectors. 
      let neighbor = sect.neighbors[s]
      var nyceil, nyfloor: float32
      if neighbor >= 0:
        nyceil = sectors[neighbor].ceil - player.where.z
        nyfloor = sectors[neighbor].floor - player.where.z

      # Project our ceiling & floor heights into screen coordinates (Y coordinate) 
      template Yaw(y, z: untyped) : untyped = y + z*player.yaw
      let y1a = WH div 2 - int32(Yaw(yceil, tz1) * yscale1)
      let y1b = WH div 2 - int32(Yaw(yfloor, tz1) * yscale1)
      let y2a = WH div 2 - int32(Yaw(yceil, tz2) * yscale2)
      let y2b = WH div 2 - int32(Yaw(yfloor, tz2) * yscale2)

      # The same for the neighboring sector 
      let ny1a = WH div 2 - int32(Yaw(nyceil, tz1) * yscale1)
      let ny1b = WH div 2 - int32(Yaw(nyfloor, tz1) * yscale1)
      let ny2a = WH div 2 - int32(Yaw(nyceil, tz2) * yscale2)
      let ny2b = WH div 2 - int32(Yaw(nyfloor, tz2) * yscale2)

      # Render the wall. 
      let beginx = max(x1, now.sx1)
      let endx = min(x2, now.sx2)
      for x in beginx..endx:
        # Calculate the Z coordinate for this point. (Only used for lighting.) 
        let z = int32(float32(x - x1) * (tz2-tz1) / float32(x2 - x1) + tz1) * 8;
        # Acquire the Y coordinates for our ceiling & floor for this X coordinate. Clamp them. 
        let ya = (x - x1) * (y2a-y1a) div (x2-x1) + y1a
        let cya = clamp(ya, ytop[x], ybottom[x])
        let yb = (x - x1) * (y2b-y1b) div (x2 - x1) + y1b
        let cyb = clamp(yb, ytop[x], ybottom[x])

        # Render ceiling: everything above this sector's ceiling height. 
        vline(x, ytop[x], cya-1, CeilingTopBorder, CeilingColor, CeilingBottomBorder)
        # Render floor: everything below this sector's floor height. 
        vline(x, cyb+1, ybottom[x], FloorTopBorder, FloorColor, FloorBottomBorder)

        # Is there another sector behind this edge? 
        if neighbor >= 0:
          # Same for _their_ floor and ceiling 
          let nya = (x - x1) * (ny2a - ny1a) div (x2-x1) + ny1a
          let cnya = clamp(nya, ytop[x], ybottom[x])
          let nyb = (x - x1) * (ny2b - ny1b) div (x2 - x1) + ny1b
          let cnyb = clamp(nyb, ytop[x], ybottom[x])
          # If our ceiling is higher than their ceiling, render upper wall 
          let r1 = WallBaseColor * (255-z)
          let r2 = BottomWallBaseColor * (31-(z div 8))
          vline(x, cya, cnya-1, WallTopBorder, if x == x2 or x == x1: WallTopBorder else: r1, WallBottomBorder)
          ytop[x] = clamp(max(cya, cnya), ytop[x], WH - 1)
          # If our floor is lower than their floor, render bottom wall 
          vline(x, cnyb+1, cyb, BottomWallTopBorder, if x == x1 or x == x2: BottomWallTopBorder else: r2, BottomWallBottomBorder)
          ybottom[x] = clamp(min(cyb, cnyb), 0, ybottom[x])
        else:
          # There's no neighbor. Render wall from top (cya = ceiling level) to bottom (cyb = floor level). 
          let r = WallBaseColor * (255-z)
          vline(x, cya, cyb, WallTopBorder, if x == x1 or x == x2: WallTopBorder else: r, WallBottomBorder)

      # Schedule the neighboring sector for rendering within the window formed by this wall. 
      if neighbor >= 0 and endx > beginx and not queueFull():
        enqueue(Item(sectorno: neighbor, sx1: beginx, sx2: endx))

    renderedsectors[now.sectorno] += 1
    if justOneSector:
      echo &"Drew sector {now[]}"
      break
{.pop.}

proc MovePlayer(dx, dy: float32) = 
  let px = player.where.x
  let py = player.where.y

  # Check if this movement crosses one of this sector's edges
  # that have a neighboring sector on the other side.
  # Because the edge vertices of each sector are defined in
  # clockwise order, PointSide will always return -1 for a point
  # that is outside the sector and 0 or 1 for a point that is inside.
  let sect = sectors[player.sector].addr
  template vert : untyped = sect.vertex
  for s in 0..<int(sect.npoints):
    if (sect.neighbors[s] >= 0 and 
        IntersectBox(px, py, px+dx, py+dy, vert[s+0].x, vert[s+0].y, vert[s+1].x, vert[s+1].y) and 
        PointSide(px+dx, py+dy, vert[s+0].x, vert[s+0].y, vert[s+1].x, vert[s+1].y) < 0):
      player.sector = uint16(sect.neighbors[s])
      break

  player.where.x += dx
  player.where.y += dy
  player.anglesin = sin(player.angle)
  player.anglecos = cos(player.angle)

proc RunLoop(gls: GLState) = 
  type Renderer = enum
    rOriginal, rOpenGL

  var mode = rOriginal
  var wsad: array[4,bool]
  var ground, falling, moving, ducking: bool
  var running = true
  var yaw = 0.0f
  var ev = Event(kind: QuitEvent)
  var avgTicks = uint32(0)
  var callDraw = drawMode == FullSpeed
  var doMovement: bool

  # Keep mouse in center of window.
  warpMouseInWindow(window, WW div 2, WH div 2)

  falling = true
  while running:
    let startT = getTicks()

    if callDraw:
      case mode
      of rOriginal:
        discard lockSurface(surface)
        DrawScreen(justOneSector = drawMode == SectorAtATime)
        unlockSurface(surface)
        discard updateSurface(window)

      of rOpenGL:
        DrawScreenGL(gls, justOneSector = drawMode == SectorAtATime)

      if drawMode == SectorAtATime:
        callDraw = false

    if queueEmpty():
      # Frame was finished
      doMovement = true
      if drawMode == SectorAtATime:
        echo "Finished frame, back to full speed."
      drawMode = FullSpeed
      callDraw = true

    # Vertical collision detection 
    if doMovement:
      let eyeheight = if ducking: DuckHeight else: EyeHeight
      ground = not falling
      if falling:
        player.velocity.z -= 0.05f
        let nextz = player.where.z + player.velocity.z
        if player.velocity.z < 0 and nextz < sectors[player.sector].floor + eyeheight:
          # Fix to ground 
          player.where.z = sectors[player.sector].floor + eyeheight
          player.velocity.z = 0
          falling = false
          ground = true
        elif player.velocity.z > 0 and nextz > sectors[player.sector].ceil:
          # Prevent jumping above ceiling
          player.velocity.z = 0
          falling = true

        if falling:
          player.where.z += player.velocity.z
          moving = true

      if moving:
        # Horizontal collision detection 
        let px = player.where.x
        let py = player.where.y
        var dx = player.velocity.x
        var dy = player.velocity.y
        let sect = sectors[player.sector].addr
        template vert : untyped = sect.vertex

        # Check if the player is about to cross one of the sector's edges 
        for s in 0..<int(sect.npoints):
          if (IntersectBox(px, py, px+dx, py+dy, vert[s+0].x, vert[s+0].y, vert[s+1].x, vert[s+1].y) and 
              PointSide(px+dx, py+dy, vert[s+0].x, vert[s+0].y, vert[s+1].x, vert[s+1].y) < 0):
            # Check where the hole is. 
            let hole_low = if sect.neighbors[s] < 0: 9e9f else: max(sect.floor, sectors[sect.neighbors[s]].floor)
            let hole_high = if sect.neighbors[s] < 0: -9e9f else: min(sect.ceil, sectors[sect.neighbors[s]].ceil)

            # Check whether we're bumping into a wall. 
            if hole_high < player.where.z + HeadMargin or hole_low > player.where.z - eyeheight+KneeHeight:
              # Bumps into a wall! Slide along the wall. 
              # This formula is from Wikipedia article "vector projection". 
              let xd = vert[s+1].x - vert[s+0].x
              let yd = vert[s+1].y - vert[s+0].y
              dx = xd * (dx*xd + yd*dy) / (xd*xd + yd*yd)
              dy = yd * (dx*xd + yd*dy) / (xd*xd + yd*yd)
              moving = false

        MovePlayer(dx, dy)
        falling = true

    while pollEvent(ev):
      if ev.kind == QuitEvent:
        running = false
      elif ev.kind in [KeyUp, KeyDown]:
        case ev.key.keysym.sym
        of K_W:
          wsad[0] = ev.kind == KeyDown
        of K_S:
          wsad[1] = ev.kind == KeyDown
        of K_A:
          wsad[2] = ev.kind == KeyDown
        of K_D:
          wsad[3] = ev.kind == KeyDown
        of K_Q, K_ESCAPE:
          running = false
        of K_SPACE:
          if ground:
            player.velocity.z += 0.5f
            falling = true
        of K_LCTRL, K_RCTRL:
          ducking = ev.kind == KeyDown
          falling = true
        of K_TAB:
          callDraw = callDraw or ev.kind == KeyDown
        of K_PERIOD:
          if ev.kind == KeyDown:
            FOVy += 2.0f
        of K_COMMA:
          if ev.kind == KeyDown:
            FOVy -= 2.0f
        of K_t:
          if ev.kind == KeyDown and drawMode == FullSpeed:
            # Clear screen before doing one sector at a time.
            var srect: sdl2.Rect = (x: 0.int32, y: 0.int32, w: WW, h: WH)
            fillRect(surface, srect.addr, 0)
            discard lockSurface(surface)
            unlockSurface(surface)
            drawMode = SectorAtATime
        of K_m:
          if ev.kind == KeyUp:
            case mode
            of rOriginal:
              mode = rOpenGL
            of rOpenGL:
              mode = rOriginal

        else:
          discard

    if doMovement:
      var x, y: int32
      const MouseSensitivity = 0.005f
      discard getMouseState(x, y)
      x = x - WW div 2
      y = y - WH div 2
      warpMouseInWindow(window, WW div 2, WH div 2)

      player.angle += float32(x) * MouseSensitivity
      yaw = clamp(yaw + float32(y)*MouseSensitivity*4.0f, -5.0f, 5.0f)
      player.yaw = yaw - player.velocity.z*0.5f
      MovePlayer(0, 0)

      var pushing = false
      var move_vec: V2f
      if wsad[0]: 
        move_vec.x += player.anglecos*0.2f
        move_vec.y += player.anglesin*0.2f
        pushing = true
      if wsad[1]:
        move_vec.x -= player.anglecos*0.2f
        move_vec.y -= player.anglesin*0.2f
        pushing = true
      if wsad[2]:
        move_vec.x += player.anglesin*0.2f
        move_vec.y -= player.anglecos*0.2f
        pushing = true
      if wsad[3]:
        move_vec.x -= player.anglesin*0.2f
        move_vec.y += player.anglecos*0.2f
        pushing = true

      let acceleration = if pushing: 0.4f else: 0.2f

      player.velocity.x = player.velocity.x * (1-acceleration) + move_vec.x * acceleration
      player.velocity.y = player.velocity.y * (1-acceleration) + move_vec.y * acceleration

      if pushing:
        moving = true

    let endT = getTicks()
    let td = endT - startT
    avgTicks = (avgTicks + td) div 2
    delay(10)

  echo &"avgTicks={avgTicks}"

proc go() = 
  var allRes: ResourceSet
  let imgflags = IMG_INIT_PNG.cint

  assert sdl2.init(INIT_VIDEO) == SdlSuccess, $sdl2.getError()
  assert image.init(imgflags) == imgflags, $sdl2.getError()

  window = glsupport.Init(allRes) do () -> WindowPtr:
    createWindow("Renderer Example", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, WW, WH, SDL_WINDOW_OPENGL)

  assert window != nil, $sdl2.getError()

  surface = getSurface(window)
  assert surface != nil, $sdl2.getError()

  let gls = NewGLState(allRes)

  showCursor(false)
  vfont.Init()

  try:
    var fn = "map-clear.txt"
    if paramCount() > 0:
      fn = paramStr(1)

    LoadData(fn)
    RunLoop(gls)
    echo GC_getStatistics()
    zstats.PrintReport()
  except:
    echo "UNCAUGHT EXCEPTION"
    var e = getCurrentException()
    while not e.isNil:
      echo(&"{e.msg}\n{e.getStackTrace()}")
      echo "---------"
      e = e.parent
  finally:
    destroyWindow(window)
    sdl2.quit()


go()
