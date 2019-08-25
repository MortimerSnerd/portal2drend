import 
  geom, math, parseutils, sdl2, random, strformat

const
  WW = int32(608)
  WH = int32(480)
  EyeHeight = 6.0f
  DuckHeight = 2.5f
  HeadMargin = 1
  KneeHeight = 2
  hfov = 0.73f*float32(WH)
  vfov = 0.2f*float32(WH)

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

proc LoadData() = 
  let fp = open("map-clear.txt", fmRead)
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
  else:
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
  ## Vars persisted over multiple calls to DrawScreen when
  ## drawing one sector at a time.

{.push checks: off.}
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
        vline(x, ytop[x], cya-1, 0x111111, 0x222222, 0x11111)
        # Render floor: everything below this sector's floor height. 
        vline(x, cyb+1, ybottom[x], 0x0000ff, 0x0000aa, 0x0000ff)

        # Is there another sector behind this edge? 
        if neighbor >= 0:
          # Same for _their_ floor and ceiling 
          let nya = (x - x1) * (ny2a - ny1a) div (x2-x1) + ny1a
          let cnya = clamp(nya, ytop[x], ybottom[x])
          let nyb = (x - x1) * (ny2b - ny1b) div (x2 - x1) + ny1b
          let cnyb = clamp(nyb, ytop[x], ybottom[x])
          # If our ceiling is higher than their ceiling, render upper wall 
          let r1 = 0x010101 * (255-z)
          let r2 = 0x040007 * (31-(z div 8))
          vline(x, cya, cnya-1, 0, if x == x2 or x == x1: 0 else: r1, 0)
          ytop[x] = clamp(max(cya, cnya), ytop[x], WH - 1)
          # If our floor is lower than their floor, render bottom wall 
          vline(x, cnyb+1, cyb, 0, if x == x1 or x == x2: 0 else: r2, 0)
          ybottom[x] = clamp(min(cyb, cnyb), 0, ybottom[x])
        else:
          # There's no neighbor. Render wall from top (cya = ceiling level) to bottom (cyb = floor level). 
          let r = 0x010101 * (255-z)
          vline(x, cya, cyb, 0, if x == x1 or x == x2: 0 else: r, 0)

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

proc RunLoop() = 
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
      discard lockSurface(surface)
      DrawScreen(justOneSector = drawMode == SectorAtATime)
      unlockSurface(surface)
      discard updateSurface(window)

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
        of K_t:
          if ev.kind == KeyDown and drawMode == FullSpeed:
            # Clear screen before doing one sector at a time.
            var srect: sdl2.Rect = (x: 0.int32, y: 0.int32, w: WW, h: WH)
            fillRect(surface, srect.addr, 0)
            discard lockSurface(surface)
            unlockSurface(surface)
            drawMode = SectorAtATime

        else:
          discard

    if doMovement:
      var x, y: int32
      discard getMouseState(x, y)
      x = x - WW div 2
      y = y - WH div 2
      warpMouseInWindow(window, WW div 2, WH div 2)

      player.angle += float32(x) * 0.03f
      yaw = clamp(yaw + float32(y)*0.05f, -5.0f, 5.0f)
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
  randomize()
  assert sdl2.init(INIT_VIDEO) == SdlSuccess, $sdl2.getError()

  window = createWindow("Renderer Example", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, WW, WH, 0)
  assert window != nil, $sdl2.getError()

  surface = getSurface(window)
  assert surface != nil, $sdl2.getError()

  showCursor(false)

  try:
    LoadData()
    RunLoop()
  finally:
    destroyWindow(window)
    sdl2.quit()


go()
