import
    fenv, strutils, math

type
    V2X*[T] = tuple[x, y: T]
    V2*     = V2X[float64]
    V2f*    = V2X[float32]
    V2i*    = V2X[int]

    AABB2X*[S] = object
        topLeft*: V2X[S]
        extent*: V2X[S]

    AABB2*     = AABB2X[float64]
    AABB2f*    = AABB2X[float32]
    AABB2i*    = AABB2X[int]

    Line2X*[T] = tuple[start: V2X[T]; extent: V2X[T]]
    Line2*     = Line2X[float64]
    Line2f*    = Line2X[float32]

    Seg2X*[T] = Line2X[T]             # Again, different name for intent only.
    Seg2*     = Seg2X[float64]
    Seg2f*    = Seg2X[float32]

    V3X*[T] = tuple[x, y, z: T]
    V3*     = V3X[float64]
    V3f*    = V3X[float32]

    P2X*[T] = tuple[r, th: T] ## Polar coordinate.
    P2f*    = P2X[float32]


    ## 4x4 matrix. Memory is laid out in column major order, 
    ## so these can be placed in OpenGL uniform buffers. 
    Matrix4x4X*[T] = distinct array[16, T]
    Matrix4x4*     = Matrix4x4X[float64]
    Matrix4x4f*    = Matrix4x4X[float32]

    Seg3X*[T] = tuple[start: V3X[T]; extent: V3X[T]]
    Seg3*     = Seg3X[float64]
    Seg3f*    = Seg3X[float32]

    V4X*[T] = tuple[x, y, z, w: T]
    V4*     = V4X[float64]
    V4f*    = V4X[float32]

    PlaneX*[T] = V4X[T]
    Plane*     = PlaneX[float64]
    Planef*    = PlaneX[float32]

    RelPosition* = enum ## Primitive test: on positive side, negative side, or coincident/intersecting.
        rpPositive, rpNegative, rpCoincident

    #TODO revisit
    Interval*[T] = object
        ## Range, where lo is inclusive, and hi is exclusive. 
        lo*, hi*: T

    IntersectResult*[T] = tuple[found: bool; s1, s2: T]

    CollideResult*[T] = object
        collided*: bool
        closestPoint*: V2X[T]

        # Vector going from closestPoint to the edge of the circle.
        overlapVector*: V2X[T]

    Degenerate = object of RangeError
        ## Degenerate case, or you've hit the partial part of a partial function.
        discard

    Edge* = object
        i1*, i2*: int       # Edge indices into vertex array, where i1 < i2
        tr1*, tr2*: int     # Indices into an index array for the two triangles that share the edge. -1 for not found.


let
  Unit3Xf* = (1.0f, 0.0f, 0.0f)
  Unit3Yf* = (0.0f, 1.0f, 0.0f)
  Unit3Zf* = (0.0f, 0.0f, 1.0f)
  Unit4Xf* = (1.0f, 0.0f, 0.0f, 0.0f)
  Unit4Yf* = (0.0f, 1.0f, 0.0f, 0.0f)
  Unit4Zf* = (0.0f, 0.0f, 1.0f, 0.0f)
  
  Origin4f* = (0.0f, 0.0f, 0.0f, 0.0f)
  Origin3f* = (0.0f, 0.0f, 0.0f)

proc vec2*[T](x, y: T) : V2X[T] = 
  ## Note: probably shouldn't need this, but sometimes
  ## (x: 1, y: 1) won't be recognized as the same as
  ## V2X[int]
  (x: x, y: y)

proc vec4*[T](v: V3X[T]; w: T) : V4X[T] =
    (v.x, v.y, v.z, w)

proc vec4*[T](v: V2X[T]; z, w: T) : V4X[T] = 
    (v.x, v.y, z, w)

proc vec4*[T](x, y, z, w: T) : V4X[T] {.inline.} = 
  (x, y, z, w)

proc vec4*[T](b: (T, T, T, T)) : V4X[T] {.inline.} = 
  b

proc clamp*[T](a: T) : T =
    ## Clamps 'a' to range [0,1]
    result = if a < T(0):
                T(0)
             elif a > T(1):
                 T(1)
             else:
                 a

proc bounds*[T](c: V2X[T]; radius: T) : AABB2X[T] =
    let
        hr = radius / T(2.0)
        hrv = (hr, hr)

    result = AABB2X[T](topLeft: c - hrv,
                           extent: hrv)

proc width*[T](bb: AABB2X[T]) : T = bb.extent.x
  ## Width of bounding box.

proc movedBy*[T](bb: AABB2X[T]; a: V2X[T]) : AABB2X[T] = 
  ## Returns copy of bb moved by adding ``a`` to ``topLeft``.
  (bb.topLeft + a) @ bb.extent

proc height*[T](bb: AABB2X[T]) : T = bb.extent.y
  ## Height of bounding box.

proc isDegenerate*[T](bb: AABB2X[T]) : bool = 
  ## Returns true if a bounding box is badly defined, and has an area <= 0.
  bb.extent.x <= T(0) or bb.extent.y <= T(0)

proc `@`*[T](topLeft: V2X[T]; extent: V2X[T]) : AABB2X[T] =
    ## BB constructor:  (1, 3) @ (10, 10)
    AABB2X[T](topLeft: topLeft, extent: extent)

proc to*[T](a, b: V2X[T]) : AABB2X[T] =
    ## AABB constructor: (10, 10).to(20, 20)
    let
      lx = min(a.x, b.x)
      ly = min(a.y, b.y)
      gx = max(a.x, b.x)
      gy = max(a.y, b.y)

    result = AABB2X[T](topLeft: (lx, ly), extent: (gx-lx, gy-ly))

proc center*[T](bb: AABB2X[T]) : V2X[T] =
    when compiles(bb.extent div T(2)):
      bb.topLeft + bb.extent div T(2)
    else:
      bb.topLeft + bb.extent / T(2)

proc `center=`*[T](bb: var AABB2X[T]; p: V2X[T]) = 
  ## Moves bb so ``p`` is the center of the box.  Leaves ``extent``
  ## unchanged. Undefined if ``bb.isDegenerate``.
  when compiles(bb.extent div T(2)):
    let hext = bb.extent div T(2)
  else:
    let hext = bb.extent * T(0.5)

  bb.topLeft = p - hext

proc `bottomLeft=`*[T](bb: var AABB2X[T]; p: V2X[T]) = 
  ## Moves ``bb`` so ``p`` is the bottom left corner, leaving ``extent``
  ## unchanged.  Undefined if ``bb.isDegenerate``.
  bb.topLeft = (p.x, p.y - bb.extent.y)

proc topRight*[T](bb: AABB2X[T]) : V2X[T] = 
  ## Returns top right corner of bb.
  (bb.topLeft.x + bb.extent.x, bb.topLeft.y)

proc `topRight=`*[T](bb: var AABB2X[T]; p: V2X[T]) = 
  ## Sets top right corner of bb to ``p``, leaving the
  ## extent alone.
  bb.topLeft = (p.x - bb.extent.x, p.y)

proc union*[T](a, b: AABB2X[T]) : AABB2X[T] =
    if isDegenerate(a):
        b
    elif isDegenerate(b):
        a
    else:
        let
            bra = a.bottomRight
            brb = b.bottomRight
            tl = (min(a.topLeft.x, b.topLeft.x), min(a.topLeft.y, b.topLeft.y))
            br = (max(bra.x, brb.x), max(bra.y, brb.y))

        AABB2X[T](topLeft: tl, extent: br - tl)

proc grownBy*[T](bb: AABB2X[T]; sz: V2X[T]) : AABB2X[T] =
    ## Expands all sides of the bounding box out by sz.
    ## Effectively increases the extent by 2*sz while leaving
    ## the center of the bounding box in the same place.
    AABB2X[T](topLeft: bb.topLeft - sz,
             extent: bb.extent + sz * T(2))

proc toOrigin*[T](bb: AABB2X[T]) : AABB2X[T] =
    AABB2X[T](topLeft: (T(0), T(0)), extent: bb.extent)

proc area*[T](bb: AABB2X[T]) : auto =
    bb.extent.x * bb.extent.y

proc bottomRight*[T](bb: AABB2X[T]) : V2X[T] =
    bb.extent + bb.topLeft

proc `bottomRight=`*[T](bb: var AABB2X[T]; p: V2X[T]) = 
  ## Sets the bottom right of ``bb`` to be ``p``, leaving the
  ## extent alone.
  bb.topLeft = p - bb.extent

proc bottomLeft*[T](bb: AABB2X[T]) : V2X[T] =
    bb.topLeft + (T(0), bb.extent.y)

proc splitX*[T](bb: AABB2X[T]; splitPos: T) : (AABB2X[T], AABB2X[T]) = 
  ## Splits a bounding box into two bounding boxes along the Y axis
  ## at X = splitPos.  Returns (leftBB, rightBB).  If splitPos is outside 
  ## of the bounding box, one of the returned boxes will be degenerate.
  result = (bb.topLeft @ (splitPos - bb.topLeft.x, bb.extent.y), 
            (splitPos, bb.topLeft.y) @ (bb.topLeft.x + bb.extent.x - splitPos, bb.extent.y))

proc splitY*[T](bb: AABB2X[T]; splitPos: T) : (AABB2X[T], AABB2X[T]) = 
  ## Splits a bounding box into two bounding boxes along the X axis
  ## at Y = splitPos.  Returns (topBB, bottomBB).  If splitPos is outside 
  ## of the bounding box, one of the returned boxes will be degenerate.
  result = (bb.topLeft @ (bb.extent.x, splitPos - bb.topLeft.y), 
            (bb.topLeft.x, splitPos) @ (bb.extent.x, bb.topLeft.y + bb.extent.y - splitPos))

proc floor*[T](p: V2X[T]) : V2X[T] = (floor(p.x), floor(p.y))
  ## Math floor() of all components of the vector.

proc contains*[T](bb: AABB2X[T]; p: V2X[T]) : bool =
    let br = bb.bottomRight

    result = p.x >= bb.topLeft.x and p.x < br.x and
             p.y >= bb.topLeft.y and p.y < br.y

proc contains*[T](a, b: AABB2X[T]) : bool =
    ## True iff a completely contains b.
    ## Just overlapping does not count.
    let
        abr = a.bottomRight
        bbr = b.bottomRight

    result = b.topLeft.x >= a.topLeft.x and
             b.topLeft.y >= a.topLeft.y and
             bbr.x < abr.x and
             bbr.y < abr.y


iterator segments*[T](s: AABB2X[T]) : Seg2X[T] =
    ## Iterates segments that make up the AABB.
    if not isDegenerate(s):
        let br = s.bottomRight

        template pp(x, y: untyped) : V2X[T] = (x, y) # For type hinting

        yield (s.topLeft, (s.extent.x, T(0)))
        yield (pp(br.x, s.topLeft.y), pp(T(0), s.extent.y))
        yield (br, (-s.extent.x, T(0)))
        yield (pp(s.topLeft.x, br.y), pp(T(0), -s.extent.y))

proc stretch*[T](bb: var AABB2X[T]; pt: V2X[T]) =
    ## If pt is outside of bb, stretches the BB out to just
    ## include it.
    bb.topLeft = (min(bb.topLeft.x, pt.x), min(bb.topLeft.y, pt.y))
    var br = bb.bottomRight

    br = (max(br.x, pt.x), max(br.y, pt.y))
    bb.extent = br - bb.topLeft

# Point functions
proc `-`*[T](v: V4X[T]) : V4X[T] = 
  (-v.x, -v.y, -v.z, -v.w)

proc `+=`*[T](a: var V4X[T]; b: V4X[T]) = 
  a.x += b.x
  a.y += b.y
  a.z += b.z
  a.w += b.w

proc `-=`*[T](a: var V4X[T]; b: V4X[T]) = 
  a.x -= b.x
  a.y -= b.y
  a.z -= b.z
  a.w -= b.w

proc `/`*[T](v: V4X[T]; s: T) : V4X[T] = 
  (v.x/s, v.y/s, v.z/s, v.w/s)

proc magSquared*[S](p: V4X[S]) : auto =
  ## Calculates the squared magnitude of a vector.  
  ## Useful for avoiding calls to sqrt when you just
  ## need to compare vectors.
  runnableExamples:
    doAssert (4, 5, 0, 0).magSquared == (16 + 25)
  p.x * p.x + p.y * p.y + p.z * p.z + p.w * p.w

proc magnitude*[S](v: V4X[S]) : auto =
    sqrt(v.magSquared)

proc normalized*[S](v: V4X[S]) : V4X[S] =
    v / v.magnitude

proc cvt*[T, D](p: V2X[T]) : V2X[D] =
    ## Converts a point to another underlying type.
    (D(p.x), D(p.y))

proc cvt*[T, D](p: V3X[T]) : V3X[D] =
    ## Converts a point to another underlying type.
    (D(p.x), D(p.y), D(p.z))

proc cvt*[T,D](a: AABB2X[T]) : AABB2X[D] =
    ## Converts a bounding box to another underlying type.
    cvt[T,D](a.topLeft) @ cvt[T,D](a.extent)

template f32*(bb: AABB2X) : AABB2X[float32] = 
    AABB2X[float32](topLeft: (bb.topLeft.x.float32, bb.topLeft.y.float32), 
                    extent: (bb.extent.x.float32, bb.extent.y.float32))
template f32*(p: V2X) : V2X[float32] = 
  (float32(p.x), float32(p.y))

template f64*(p: V2X) : V2X[float64] = 
  (float64(p.x), float64(p.y))

proc `div`*[T](a: V2X[T]; b: T) : V2X[T] =
    (a.x div b, a.y div b)

proc `div`*[T](a: V2X[T]; b: V2X[int]) : V2X[T] =
    (a.x div b.x, a.y div b.y)

proc `-`*[T](a: V2X[T]) : V2X[T] =
    (-a.x, -a.y)

proc `-`*[T](a:V3X[T]) : V3X[T] =
    (-a.x, -a.y, -a.z)

proc `+`*[T](a, b: V2X[T]) : V2X[T] =
    (a.x + b.x, a.y + b.y)

proc `+`*[T](a, b: V3X[T]) : V3X[T] =
    (a.x + b.x, a.y + b.y, a.z + b.z)

proc `+`*[T](a, b: V4X[T]) : V4X[T] = 
  (a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w)

proc `+=`*[T](a: var V2X[T]; b: V2X[T]) =
    a = a + b

proc `+=`*[T](a: var V3X[T]; b: V3X[T]) =
    a = a + b

proc `-=`*[T](a: var V3X[T]; b: V3X[T]) =
    a = a - b

proc `-=`*[T](a: var V2X[T]; b: V2X[T]) =
    a = a - b

proc `+`*[T](a: V2X[T], b: T) : V2X[T] =
    (a.x + b, a.y + b)

proc `+`*[T](a: V3X[T], b: T) : V3X[T] =
    (a.x + b, a.y + b, a.z + b)

proc `+`*[T](a: T; b: V2X[T]) : V2X[T] =
    (a + b.x, a + b.y)

proc `+`*[T](a: T; b: V3X[T]) : V3X[T] =
    (a + b.x, a + b.y, a + b.z)

proc `-`*[T](a, b: V2X[T]) : V2X[T] =
    (a.x - b.x, a.y - b.y)

proc `-`*[T](a, b: V3X[T]) : V3X[T] =
    (a.x - b.x, a.y - b.y, a.z - b.z)

proc `-`*[T](a: T; b: V2X[T]) : V2X[T] =
    (a - b.x, a - b.x)

proc `-`*[T](a: V3X[T], b: T) : V3X[T] =
    (a.x - b, a.y - b, a.z - b)

proc `-`*[T](a, b: V4X[T]) : V4X[T] = 
    (a.x - b.x, a.y - b.y, a.z - b.z, a.w - b.w)

proc `*=`*[T](a: var V2X[T]; b: float) =
    a.x = a.x * b
    a.y = a.y * b

proc `-`*[T](a: V2X[T], b: T) : V2X[T] =
    (a.x - b, a.y - b)

proc `*`*[T,S](a: V3X[T]; b: V3X[S]) : auto =
    (a.x * b.x, a.y*b.y, a.z*b.z)

proc `*`*[T, S](a: V3X[T]; b: S) : auto =
    (a.x * b, a.y * b, a.z * b)

proc `*`*[T,S](a: V2X[T]; b: V2X[S]) : auto =
    (a.x * b.x, a.y * b.y)

proc `*`*[T, S](a: V2X[T]; b: S) : auto =
    (a.x * b, a.y * b)

proc `*`*[T, S](b: S; a: V2X[T]) : auto =
    (a.x * b, a.y * b)

proc `*`*[T, S](b: S; a: V3X[T]) : auto =
    (a.x * b, a.y * b, a.z * b)

proc `*`*[T, S](a: V4X[T]; b: S) : auto = 
  (a.x*b, a.y*b, a.z*b, a.w*b)

proc `*`*[T](a, b: V4X[T]) : V4X[T] = 
  (a.x * b.x, a.y * b.y, a.z * b.z, a.w * b.w)

proc `/`*[T, S](a: V3X[T]; b: S) : auto =
    (a.x / b, a.y / b, a.z / b)

proc `/`*[T, S](a: V2X[T]; b: S) : auto =
    (a.x / b, a.y / b)

proc `/`*[T, S](a: T; b: V3X[S]) : auto =
    (a / b.x, a / b.y, a / b.z)

proc `/`*[T, S](a: T; b: V2X[S]) : auto =
    (a / b.x, a / b.y)

proc min*[T](a: V2X[T]) : T =
  min(a.x, a.y)

proc max*[T](a: V2X[T]) : T =
  max(a.x, a.y)

proc magSquared*[S](p: V2X[S]) : auto =
    p.x * p.x + p.y * p.y

proc magSquared*[S](p: V3X[S]) : auto =
    p.x * p.x + p.y * p.y + p.z * p.z

proc magnitude*[S](v: V2X[S]) : auto =
    S(sqrt(v.magSquared))

proc magnitude*[S](v: V3X[S]) : auto =
    S(sqrt(v.magSquared))

proc normalized*[S](v: V2X[S]) : V2X[S] =
    v / v.magnitude

proc normalized*[S](v: V3X[S]) : V3X[S] =
    v / v.magnitude

proc determinant*[S](a, b: V2X[S]) : auto =
    ## 2x2 determinant where the vectors are mapped to columns of the matrix.
    a.x * b.y - b.x * a.y

proc dot*[S,T](a: V4X[S]; b: V4X[T]) : S =
    a.x*b.x + a.y*b.y + a.z*b.z + a.w*b.w

proc dot*[S,T](a: openarray[S]; b: openarray[T]) : S =
    assert(len(a) == len(b))

    for i in 0..<len(a):
        result += a[i] * b[i]

proc dot*[S](a, b: V2X[S]) : S = 
    a.x * b.x + a.y * b.y

proc dot*[S](a, b: V3X[S]) : S = 
    a.x * b.x + a.y * b.y + a.z * b.z

proc lerp*[P,S](a, b: P, v: S) : P =
    ## Linear interpolation
    a + (b - a) * v

proc normalizeAngle*[T](a: T) : T = 
  ## Re-normalizes an angle back the the ``-PI..PI`` range.
  result = a
  while result < PI:
    result += T(2) * PI

  while result > PI:
    result -= T(2) * PI 


proc lerpAngle*[T](a, b, t: T) : T = 
  ## Lerps between angles, keeping it between -PI and PI
  ## Not defined if ``a`` or ``b`` is out of the ``PI..-PI`` range.
  var start = a
  var stop  = b

  if abs(start - stop) > PI:
    if stop < T(0):
      stop += T(2) * PI
    elif start < T(0):
      start += T(2) * PI

  return normalizeAngle(lerp(start, stop, t))

proc rescale*[T](lo, hi, v: T) : T = 
  ## Rescales ``v``  to be between 0 and 1 if it is between ``lo`` and ``hi``.
  (v-lo) / (hi - lo)

proc area*[T](v: V2X[T]) : auto =
   v.x * v.y

proc angle*[T](x, y: T) : T =
    arctan2(y, x)

proc cross*[T](a, b: V3X[T]) : V3X[T] =
    (x: a.y*b.z - b.y*a.z,
     y: a.z*b.x - b.z*a.x,
     z: a.x*b.y - b.x*a.y)

proc vec3*[T](xy: V2X[T]; z: T) : V3X[T] =
    (xy.x, xy.y, z)

proc vec3*[T](v: V4X[T]) : V3X[T] = 
  ## Truncates a V4 into a V3.
  (v.x, v.y, v.z)

template vec3*[T](x0, y0, z0: T) : V3X[T] = (x: x0, y: y0, z: z0)

proc cross*[T](a, b: V2X[T]) : auto =
    a.x * b.y - a.y * b.x

proc endPoint*[T](s: Seg2X[T]) : V2X[T] =
    s.start + s.extent

proc perpTo*[T](s: V2X[T]) : V2X[T] =
    ## Returns vector perpindicular to s
    (-s.y, s.x)

proc trianglePlane*[T](p1, p2, p3: V3X[T]) : PlaneX[T] =
    ## Returns plane as defined by the 3 points, with the normal
    ## pointing out from a CCW winding.
    let n = cross(p2-p1, p3-p1)

    result = (n.x, n.y, n.z, -dot(n, p1))

proc normal*[T](p: PlaneX[T]) : V3X[T] =
    ## The probably unnormalized normal for the plane.
    (p.x, p.y, p.z)

proc seg2*[T](start, endp: V2X[T]) : Seg2X[T] =
    ## Creates a segment from two endpoints.
    (start, endp - start)

proc growBy*[T](s: Seg2X[T]; ln: T) : Seg2X[T] =
    ## Grows or shrinks the segment so the start point
    ## is fixed, and the endpoint pulls in/pushes out.
    ## It will flip the segments' direction if ln > magnitude(s)
    let
        m = magnitude(s.extent)
        nExt = s.extent / m

    result = (s.start, nExt * (m+ln))

proc midpoint*[T](s: Seg2X[T]) : V2X[T] =
    ## Midpoint of a line segment.
    s.start + s.extent / T(2)

proc toPolar*[T](p: V2X[T]) : P2X[T] = 
  ## Converts p to a polar coordinate relative to origin (0, 0). 
  ## Keeps the angle between 0 and 2PI.
  let th = arctan2(p.y, p.x)
  if th < T(0):
    (magnitude(p), th + T(2)*PI)
  else:
    (magnitude(p), th)

proc opposite*(side: RelPosition) : RelPosition = 
  ## Returns the opposite side of 'side'.  
  ## Identity for rpCoincident.
  case side
  of rpPositive:
    rpNegative
  of rpNegative: 
    rpPositive
  of rpCoincident:
    rpCoincident

proc classify*[T](ln: Seg2X[T]; p: V2X[T]; eps = 0.00001) : RelPosition =
    let det = cross(ln.extent, p - ln.start)

    if det > -eps and det < eps:
        rpCoincident
    elif det < 0:
        rpNegative
    else:
        rpPositive

proc classify*[T](p: PlaneX[T]; v: V4X[T]; eps = 0.00001) : RelPosition = 
  ## Is vertex v on the plane, or on the negative or positive side?
  ## rpPositive -> plane's normal points towards the vertex.
  let det = dot(p, v)

  if det.almost(T(0), eps):
    result = rpCoincident
  elif det < T(0):
    result = rpNegative
  else:
    result = rpPositive

proc classify*[T](p: PlaneX[T]; v: V3X[T]; eps = 0.00001) : RelPosition = 
  ## Is vertex v on the plane, or on the negative or positive side?
  ## rpPositive -> plane's normal points towards the vertex.
  classify(p, (v.x, v.y, v.z, T(1)), eps)

proc crossesBoundary*(r1, r2: RelPosition) : bool = 
  ## Returns true if r1 -> r2 transitions from pos to neg or neg to 
  ## pos.  We don't count transitions from either to rpCoincident as crossing
  ## the boundary.
  (r1 == rpPositive and r2 == rpNegative) or (r1 == rpNegative and r2 == rpPositive)

proc bounds*[T](s: Seg2X[T], minDim = 0.0001) : AABB2X[T] =
    ## Bounding box for segment. minDim provides the minimum
    ## axis extent for vertical or horizontal lines, so we don't
    ## end up with area == 0.
    let
        ep             = s.endPoint
        tl             = (min(s.start.x, ep.x), min(s.start.y, ep.y))
        br             = (max(s.start.x, ep.x), max(s.start.y, ep.y))
        ex : V2X[T] = br - tl

    AABB2X[T](topLeft: tl,
             extent: (max(T(minDim), ex.x), max(T(minDim), ex.y)))

proc calcParametricLineIntersection*[T](l1, l2: Line2X[T]) : IntersectResult[T] =
    ## Returns true and the s parameters along the two lines if they
    ## intersect.  (ie: parametric line equation pt = start + s*extent)
    ## Returns false if the lines are parallel.  We ignore the coincident lines case.
    let
        diff1 = l1.extent
        diff2 = -l2.extent
        det = cross(diff1, diff2)

    if det.almost(0, 0.000001):
        result.found = false
    else:
        let startDiff = l1.start - l2.start

        result.s1 = cross(diff2, startDiff) / det
        result.s2 = cross(startDiff, diff1) / det
        result.found = true


proc closestPointTo*[T](s: Seg2X[T];
        p: V2X[T]) : V2X[T] {.raises: [Degenerate].} =
    ## Returns the closest point on 's' to point p.
    # Oh degenerate cases, how I loathe you.
    if s.extent.almost((T(0), T(0))):
        result = s.start
    else:
        let
            perpSeg = (p, perpTo(s.extent))
            ir = calcParametricLineIntersection(s, perpSeg)
            sFact = clamp(ir.s1)

        if ir.found:
            result = s.start + s.extent*sFact
        else:
            raise newException(Degenerate,"perp line not perp?")


proc closestPoints*[T](l1, l2: Seg2X[T]) : (V2X[T], V2X[T]) =
    ## Returns closest points between two line segments.
    ## Does not indicate whether they intersect.
    let zero = (T(0), T(0))
    var segIsPt = if l1.extent.almost(zero): 0b01 else: 0b00

    segIsPt = segIsPt or (if l2.extent.almost(zero): 0b10 else: 0b00)

    case segIsPt
    of 0b11:
        # Both are points, hooray.
        result = (l1.start, l2.start)
    of 0b10:
        let cp = l1.closestPointTo(l2.start)

        result = (cp, l2.start)
    of 0b01:
        let cp = l2.closestPointTo(l1.start)

        result = (l1.start, cp)
    else: # 0b00
        # Two actual lines
        let ir = calcParametricLineIntersection(l1, l2)

        if ir.found:
            let
                s1 = clamp(ir.s1)
                s2 = clamp(ir.s2)

            result = (l1.start + l1.extent*s1, l2.start + l2.extent*s2)
        else:
            # Lines are parallel, so calculate for each point on the
            # lines, and return the pair that are the closest.
            let l1_s2 = l1.closestPointTo(l2.start)
            var mindist = magSquared(l1_s2-l2.start)

            result = (l1_s2, l2.start)

            let
                l1_e2 = l1.closestPointTo(l2.endPoint)
                l1_e2d = magSquared(l1_e2-l2.endPoint)

            if l1_e2d < mindist:
                result = (l1_e2, l2.endPoint)
                mindist = l1_e2d

            let
                l2_s1 = l2.closestPointTo(l1.start)
                l2_s1d = magSquared(l2_s1-l1.start)

            if l2_s1d < mindist:
                result = (l1.start, l2_s1)
                mindist = l2_s1d

            let
                l2_e1 = l2.closestPointTo(l1.endPoint)
                l2_e1d = magSquared(l2_e1-l1.endPoint)

            if l2_e1d < mindist:
                result = (l1.endPoint, l2_e1)



template within*(v: untyped, a, b: untyped) : untyped =
    ## Is v within [a, b]
    v >= a and v <= b

proc intersects*[T](ir: IntersectResult[T]) : bool =
    ## Returns true if the IntersectResult from intersecting
    ## two line segments actually intersect.  Not the same as
    ## line intersection.
    ir.found and ir.s1.within(T(0), T(1)) and ir.s2.within(T(0), T(1))

proc segsIntersect*[T](a, b: Seg2X[T]) : bool =
    ## Just tests for intersection, without returning
    ## the intersection point.
    let rc = calcParametricLineIntersection(a, b)

    result = rc.intersects()

template squared*(a: untyped) : untyped =
  let x = a

  x*x

proc almost*[T](p1, p2: V2X[T]; maxDiff = 0.00001) : bool =
    ## Returns true if the points are within 'maxDiff' magnitude
    ## squared of each other.
    (p1 - p2).magSquared <= maxDiff

proc projXZ*[T](p: V3X[T]) : V2X[T] =
    ## Projects the x and z coordinates into the x and y coordinates
    ## of a Point2d
    (p.x, p.z)

proc almost*[T](p1, p2: V3X[T]; maxDiff = 0.00001) : bool =
    ## Returns true if the points are within 'maxDiff' magnitude
    ## squared of each other.
    (p1 - p2).magSquared <= maxDiff

proc almost*[T](v1, v2: T; maxDiff = 0.00001) : bool =
    ## Returns true if v1 is within the 'maxDiff' of v2.
    abs(v1-v2) <= maxDiff

proc manhattan*[T](a, b: V2X[T]) : T = 
  ## Manhattan distance between two points.
  abs(a.x - b.x) + abs(a.y - b.y)

proc leftOf*[T](pt: V2X[T]; seg: Seg2X[T]) : bool =
    ## Returns true of pt is left of the line/segment seg.
    classify(seg, pt) == rpNegative

proc rightOf*[T](pt: V2X[T]; seg: Seg2X[T]) : bool =
    ## Returns true of pt is right of the line/segment seg.
    classify(seg, pt) == rpPositive

proc furthest*[T](pt: V2X[T]; a, b: V2X[T]) : V2X[T] = 
  ## From ``a`` and ``b``, returns the point furthest from ``pt``.
  ## If the points are equidistant to ``pt``, ``a`` is returned.
  let ma = magSquared(pt - a)
  let mb = magSquared(pt - b)
  if mb > ma:
    return b

  return a

proc closest*[T](pt: V2X[T]; a, b: V2X[T]) : V2X[T] = 
  ## From ``a`` and ``b``, returns the point closest to ``pt``.
  ## If the points are equidistant to ``pt``, ``a`` is returned.
  let ma = magSquared(pt - a)
  let mb = magSquared(pt - b)
  if mb < ma:
    return b

  return a
#
# Interval functions
proc contains*[T](a: Interval[T]; t: T) : bool =
    t >= a.lo and t < a.hi

proc len*[T](a: Interval[T]) : T =
    ## Length of the interval.
    a.hi - a.lo

template ok*(a: Interval) : untyped =
    ## True if the interval is a valid interval.
    len(a) > 0

proc intersection*[T](a, b: Interval[T]) : Interval[T] =
    Interval[T](lo: max(a.lo, b.lo), hi: min(a.hi, b.hi))

proc overlaps*[T](a, b: Interval[T]) : bool =
    len(a.intersection(b)) > T(0)

proc union*[T](a, b: Interval[T]) : Interval[T] =
    Interval[T](lo: min(a.lo, b.lo), hi: max(a.hi, b.hi))

proc mkInterval*[T](hi, lo: T) : Interval[T] =
    Interval[T](lo: min(hi,lo), hi: max(lo, hi))

proc `+`*[T](i: Interval[T]; s: T) : Interval[T] =
    ## Moves the interval by 's'.
    Interval[T](lo: i.lo + s, hi: i.hi + s)

proc shortenedBy*[T](i: Interval[T]; s: T) : Interval[T] =
    ## Subtracts s from the interval length in such a
    ## way that the midpoint of the interval stays the same.
    ## It can make the len(interval) < 0.
    when compiles(s div T(2)):
      let hs = s div T(2)
    else:
      let hs = s / T(2)
    Interval[T](lo: i.lo + hs,
                hi: i.hi - hs)

proc center*[T](i: Interval[T]) : T = 
  when compiles(i.lo div T(2)):
    i.lo + (i.hi - i.lo) div T(2)
  else:
    i.lo + (i.hi - i.lo) / T(2)

proc xInterval*[T](bb: AABB2X[T]) : Interval[T] =
    ## Interval over the x axis covered by this bounding box.
    Interval[T](lo: bb.topLeft.x, hi: bb.topLeft.x + bb.extent.x)

proc yInterval*[T](bb: AABB2X[T]) : Interval[T] =
    ## Interval over the y axis covered by this bounding box.
    Interval[T](lo: bb.topLeft.y, hi: bb.topLeft.y + bb.extent.y)

proc overlaps*[T](a, b: AABB2X[T]) : bool =
    ## Do these two bounding boxes overlap?
    let
        aix = a.xInterval
        aiy = a.yInterval
        bix = b.xInterval
        biy = b.yInterval

    result = len(aix.intersection(bix)) > 0 and
             len(aiy.intersection(biy)) > 0

proc even*(n: int) : bool = (n and 1) == 0

proc intersection*[T](a, b: AABB2X[T]) : AABB2X[T] = 
  ## Returns intersection of two bounding boxes.
  ## Result is degenerate if the boxes do not overlap.
  let
    xi = a.xInterval.intersection(b.xInterval)
    yi = a.yInterval.intersection(b.yInterval)

  (xi.lo, yi.lo) @ (xi.hi-xi.lo, yi.hi-yi.lo)

proc circleSegCollision*[T](p: V2X[T];
                         radius: T;
                         seg: Seg2X[T]) : CollideResult[T] =
    let
        segLen       = magnitude(seg.extent)   # calculate just once
        ptVec        = p - seg.start
        segExtNorm   = seg.extent / segLen
        proj         = dot(ptVec, segExtNorm)
        boundedProj  = max(0.0, min(segLen, proj))
        closest      = seg.start + segExtNorm * boundedProj
        distV        = p - closest
        dist         = magnitude(distV)

    if dist <= radius:
        result.collided = true
        result.closestPoint = closest
        if dist.almost(0.T):
            # Right through the center.  Just pick a direction perpindicular
            # to the segment.
            result.overlapVector = perpTo(seg.extent.normalized) * radius
        else:
            result.overlapVector = distV.normalized * (radius - dist)

#
# Matrix functions
template `[]`*[T](m: Matrix4x4X[T], i: int) : T = array[16, T](m)[i]

template M11*[T](m: Matrix4x4X[T]) : var T = m[0]
template M12*[T](m: Matrix4x4X[T]) : var T = m[4]
template M13*[T](m: Matrix4x4X[T]) : var T = m[8]
template M14*[T](m: Matrix4x4X[T]) : var T = m[12]
template M21*[T](m: Matrix4x4X[T]) : var T = m[1]
template M22*[T](m: Matrix4x4X[T]) : var T = m[5]
template M23*[T](m: Matrix4x4X[T]) : var T = m[9]
template M24*[T](m: Matrix4x4X[T]) : var T = m[13]
template M31*[T](m: Matrix4x4X[T]) : var T = m[2]
template M32*[T](m: Matrix4x4X[T]) : var T = m[6]
template M33*[T](m: Matrix4x4X[T]) : var T = m[10]
template M34*[T](m: Matrix4x4X[T]) : var T = m[14]
template M41*[T](m: Matrix4x4X[T]) : var T = m[3]
template M42*[T](m: Matrix4x4X[T]) : var T = m[7]
template M43*[T](m: Matrix4x4X[T]) : var T = m[11]
template M44*[T](m: Matrix4x4X[T]) : var T = m[15]

proc identity3d*[T]() : Matrix4x4X[T] =
  (result.M11) = T(1)
  (result.M22) = T(1)
  (result.M33) = T(1)
  (result.M44) = T(1)


proc translation3d*[T](p: V3X[T]) : auto =
  Matrix4x4X[T]([T(1), T(0), T(0), T(0), 
                 T(0), T(1), T(0), T(0), 
                 T(0), T(0), T(1), T(0), 
                 p.x,  p.y,  p.z,  T(1)])

proc scale3d*[T](v: V3X[T]) : auto =
  Matrix4x4X[T]([v.x,  T(0), T(0), T(0), 
                 T(0), v.y,  T(0), T(0), 
                 T(0), T(0), v.z,  T(0), 
                 T(0), T(0), T(0), T(1)])

# Bravely copied and pasted from basic3d.nim
proc determinant*[T](m:Matrix4x4X[T]) : T=
  ## Computes the determinant of matrix `m`.
  let
    O1=m.M31*m.M44-m.M34*m.M41
    O2=m.M32*m.M44-m.M34*m.M42
    O3=m.M31*m.M42-m.M32*m.M41
    O4=m.M33*m.M44-m.M34*m.M43
    O5=m.M31*m.M43-m.M33*m.M41
    O6=m.M32*m.M43-m.M33*m.M42

  return (O1*m.M12-O2*m.M11-O3*m.M14)*m.M23+
    (-O1*m.M13+O4*m.M11+O5*m.M14)*m.M22+
    (O2*m.M13-O4*m.M12-O6*m.M14)*m.M21+
    (O3*m.M13-O5*m.M12+O6*m.M11)*m.M24

# Bravely copied and pasted from basic3d.nim
proc inverse*[T](m: Matrix4x4X[T]) : Matrix4x4X[T] =
   let det=m.determinant

   let idet = T(1) / det

   (result.M11) = (m.M23*m.M34*m.M42 - m.M24*m.M33*m.M42 + m.M24*m.M32*m.M43 - m.M22*m.M34*m.M43 - m.M23*m.M32*m.M44 + m.M22*m.M33*m.M44) * idet
   (result.M12) = (m.M14*m.M33*m.M42 - m.M13*m.M34*m.M42 - m.M14*m.M32*m.M43 + m.M12*m.M34*m.M43 + m.M13*m.M32*m.M44 - m.M12*m.M33*m.M44) * idet
   (result.M13) = (m.M13*m.M24*m.M42 - m.M14*m.M23*m.M42 + m.M14*m.M22*m.M43 - m.M12*m.M24*m.M43 - m.M13*m.M22*m.M44 + m.M12*m.M23*m.M44) * idet
   (result.M14) = (m.M14*m.M23*m.M32 - m.M13*m.M24*m.M32 - m.M14*m.M22*m.M33 + m.M12*m.M24*m.M33 + m.M13*m.M22*m.M34 - m.M12*m.M23*m.M34) * idet
   (result.M21) = (m.M24*m.M33*m.M41 - m.M23*m.M34*m.M41 - m.M24*m.M31*m.M43 + m.M21*m.M34*m.M43 + m.M23*m.M31*m.M44 - m.M21*m.M33*m.M44) * idet
   (result.M22) = (m.M13*m.M34*m.M41 - m.M14*m.M33*m.M41 + m.M14*m.M31*m.M43 - m.M11*m.M34*m.M43 - m.M13*m.M31*m.M44 + m.M11*m.M33*m.M44) * idet
   (result.M23) = (m.M14*m.M23*m.M41 - m.M13*m.M24*m.M41 - m.M14*m.M21*m.M43 + m.M11*m.M24*m.M43 + m.M13*m.M21*m.M44 - m.M11*m.M23*m.M44) * idet
   (result.M24) = (m.M13*m.M24*m.M31 - m.M14*m.M23*m.M31 + m.M14*m.M21*m.M33 - m.M11*m.M24*m.M33 - m.M13*m.M21*m.M34 + m.M11*m.M23*m.M34) * idet
   (result.M31) = (m.M22*m.M34*m.M41 - m.M24*m.M32*m.M41 + m.M24*m.M31*m.M42 - m.M21*m.M34*m.M42 - m.M22*m.M31*m.M44 + m.M21*m.M32*m.M44) * idet
   (result.M32) = (m.M14*m.M32*m.M41 - m.M12*m.M34*m.M41 - m.M14*m.M31*m.M42 + m.M11*m.M34*m.M42 + m.M12*m.M31*m.M44 - m.M11*m.M32*m.M44) * idet
   (result.M33) = (m.M12*m.M24*m.M41 - m.M14*m.M22*m.M41 + m.M14*m.M21*m.M42 - m.M11*m.M24*m.M42 - m.M12*m.M21*m.M44 + m.M11*m.M22*m.M44) * idet
   (result.M34) = (m.M14*m.M22*m.M31 - m.M12*m.M24*m.M31 - m.M14*m.M21*m.M32 + m.M11*m.M24*m.M32 + m.M12*m.M21*m.M34 - m.M11*m.M22*m.M34) * idet
   (result.M41) = (m.M23*m.M32*m.M41 - m.M22*m.M33*m.M41 - m.M23*m.M31*m.M42 + m.M21*m.M33*m.M42 + m.M22*m.M31*m.M43 - m.M21*m.M32*m.M43) * idet
   (result.M42) = (m.M12*m.M33*m.M41 - m.M13*m.M32*m.M41 + m.M13*m.M31*m.M42 - m.M11*m.M33*m.M42 - m.M12*m.M31*m.M43 + m.M11*m.M32*m.M43) * idet
   (result.M43) = (m.M13*m.M22*m.M41 - m.M12*m.M23*m.M41 - m.M13*m.M21*m.M42 + m.M11*m.M23*m.M42 + m.M12*m.M21*m.M43 - m.M11*m.M22*m.M43) * idet
   (result.M44) = (m.M12*m.M23*m.M31 - m.M13*m.M22*m.M31 + m.M13*m.M21*m.M32 - m.M11*m.M23*m.M32 - m.M12*m.M21*m.M33 + m.M11*m.M22*m.M33) * idet


proc rotation3d*[T](axis: V3X[T]; ang: T) : Matrix4x4X[T] =
    let
        a   = T(ang)
        c   = cos(a)
        s   = sin(a)
        iC  = T(1) - c
        x   = axis.x
        y   = axis.y
        z   = axis.z
        x2  = x * x
        y2  = y * y
        z2  = z * z

    result = Matrix4x4X[T]([x2 + (T(1) - x2)*c,  iC*x*y + z*s,      iC*x*z - y*s,      T(0), 
                            iC*x*y - z*s,        y2 + (T(1)-y2)*c,  iC*y*z + x*s,      T(0),
                            iC*x*z + y*s,        iC*y*z - x*s,      z2 + (T(1)-z2)*c,  T(0),
                            T(0),                T(0),              T(0),              T(1)])


proc `*`*[T](a, b: Matrix4x4X[T]) : Matrix4x4X[T] =
    Matrix4x4X[T]([a.M11*b.M11 + a.M12*b.M21 + a.M13*b.M31 + a.M14*b.M41,
                   a.M21*b.M11 + a.M22*b.M21 + a.M23*b.M31 + a.M24*b.M41,
                   a.M31*b.M11 + a.M32*b.M21 + a.M33*b.M31 + a.M34*b.M41,
                   a.M41*b.M11 + a.M42*b.M21 + a.M43*b.M31 + a.M44*b.M41,

                   a.M11*b.M12 + a.M12*b.M22 + a.M13*b.M32 + a.M14*b.M42,
                   a.M21*b.M12 + a.M22*b.M22 + a.M23*b.M32 + a.M24*b.M42,
                   a.M31*b.M12 + a.M32*b.M22 + a.M33*b.M32 + a.M34*b.M42,
                   a.M41*b.M12 + a.M42*b.M22 + a.M43*b.M32 + a.M44*b.M42,

                   a.M11*b.M13 + a.M12*b.M23 + a.M13*b.M33 + a.M14*b.M43,
                   a.M21*b.M13 + a.M22*b.M23 + a.M23*b.M33 + a.M24*b.M43,
                   a.M31*b.M13 + a.M32*b.M23 + a.M33*b.M33 + a.M34*b.M43,
                   a.M41*b.M13 + a.M42*b.M23 + a.M43*b.M33 + a.M44*b.M43,
                   
                   a.M11*b.M14 + a.M12*b.M24 + a.M13*b.M34 + a.M14*b.M44,
                   a.M21*b.M14 + a.M22*b.M24 + a.M23*b.M34 + a.M24*b.M44,
                   a.M31*b.M14 + a.M32*b.M24 + a.M33*b.M34 + a.M34*b.M44,
                   a.M41*b.M14 + a.M42*b.M24 + a.M43*b.M34 + a.M44*b.M44])


proc `*`*[T](a: Matrix4x4X[T]; p: V4X[T]) : V4X[T] =
    ## Multiply a x p with p as a column vector.
    ## Implication: Combined transforms need to be multiplied in 
    ##   reverse order of the desired transform order when you convert
    ##   vertices to column vectors.  So, 
    ##   S * T conceptually performs T first, then S.
    (x: (a.M11*p.x + a.M12*p.y + a.M13*p.z + a.M14*p.w),
     y: (a.M21*p.x + a.M22*p.y + a.M23*p.z + a.M24*p.w),
     z: (a.M31*p.x + a.M32*p.y + a.M33*p.z + a.M34*p.w), 
     w: a.M41*p.x + a.M42*p.y + a.M43*p.z + a.M44*p.w)

proc lookAt*[T](viewPt: V3X[T]; targetPt: V3X[T]; up: V3X[T]) : Matrix4x4X[T] =
    ## Constructs a lookAt matrix that converts world to camera space.
    ## Note: this function composes matrices in a way consistent with 
    ## treating vectors as column vectors.  It's convenient for now, as 
    ## the current shaders do Mv rather than vM.  
    let
        lookDir   = (targetPt - viewPt).normalized
        upDir     = up.normalized
        rightDir  = cross(lookDir, upDir).normalized
        perpUpDir = cross(rightDir, lookDir)
        rotMat    = Matrix4x4X[T]([rightDir.x,  perpUpDir.x, -lookDir.x,  T(0), 
                                   rightDir.y,  perpUpDir.y, -lookDir.y,  T(0), 
                                   rightDir.z,  perpUpDir.z, -lookDir.z,  T(0),
                                   T(0),        T(0),        T(0),        T(1)])
        trans     = translation3d(-viewPt)

    result = rotMat * trans

proc perspectiveProjection*[T](fov: T; zNear: T; zFar: T; aspectRatio: T) : Matrix4x4X[T] =
    ## Creates a perspective projection with the given parameters.  Assumes -z is forward
    ## in "camera" space.  
    let
        ndist = zNear - zFar
        frustumScale = T(1) / tan(fov / T(2))

    (result.M11) = frustumScale / aspectRatio
    (result.M22) = frustumScale
    (result.M33) = (zFar + zNear) / ndist
    (result.M34) = (T(2) * zFar * zNear) / ndist
    (result.M43) = T(-1)

proc orthoProjection*[T](left, right, bottom, top, znear, zfar: T) : Matrix4x4X[T] = 
  ## Creates an orthographic projection.
  (result.M11) = 2.T / (right - left)
  (result.M14) = -(right+left)/(right-left)
  (result.M22) = 2.T/(top-bottom)
  (result.M24) = -(top+bottom)/(top-bottom)
  (result.M33) = -2.T / (zfar - znear)
  (result.M34) = -(zfar+znear)/(zfar-znear)
  (result.M44) = 1.T


proc orthoProjectionYDown*[T](left, right, top, bottom, znear, zfar: T) : Matrix4x4X[T] = 
  ## Creates an orthographic projection where +Y goes down.
  assert right > left
  assert bottom > top
  assert zfar > znear

  let 
    w = right - left
    h = bottom - top
    d = zfar - znear
    
  # x = 2*(x0-left)/width - 1.0
  # y = 2*(bottom-y0)/height - 1.0
  # z = 2*(zfar - z0)/depth - 1.0 
  # This could be simplified, but I can't be assed.
  result = translation3d((-1.T, -1.T, -1.T)) * 
           scale3d((2.T/w, 2.T/h, 2.T/d)) *
           translation3d((-left, bottom, -znear)) *
           scale3d((1.T, -1.T, 1.T))

iterator triangleEdges[I](indices: openarray[I]; triIdx: int) : (I,I) =
    yield (indices[triIdx], indices[triIdx+1])
    yield (indices[triIdx+1], indices[triIdx+2])
    yield (indices[triIdx+2], indices[triIdx])

proc extractEdges*[I](indices: openarray[I]; outEdges: var seq[Edge]) =
    ## For a index array that describes a triangle mesh,
    ## extracts an array of edges that makes the triangles that share that
    ## edge explicit.  This only works if the vertices that are in the
    ## unseen vertex array are uniqye, with no duplicates.  Also assumes
    ## the indices are set up in the format required for GL_TRIANGLES.
    ## This does _not_ clear the outEdges array, so it can be used to
    ## accumulate across geometry that's guaranteed to not share any edges.
    if outEdges.isNil:
        newSeq(outEdges, len(indices))

    var workingEdges : seq[Edge] = @[]

    # The slow but simple implementation.
    for triIdx in countup(0, <len(indices), 3):
        for x1, x2 in triangleEdges(indices, triIdx):
            let
                i1 = min(x1, x2)
                i2 = max(x1, x2)
            var found = false

            for i in 0..<len(workingEdges):
                if workingEdges[i].i1 == i1 and workingEdges[i].i2 == i2:
                        # Found matching edge.
                        workingEdges[i].tr2 = triIdx
                        add(outEdges, workingEdges[i])
                        del(workingEdges, i)
                        found = true
                        break

            if not found:
                add(workingEdges, Edge(i1: i1, i2: i2, tr1: triIdx, tr2: -1))


    # Anything still left in workingEdges is an edge owned by just
    # one triangle. We stick those on the end.
    add(outEdges, workingEdges)


template rotateAroundOrigin*(p: V2X; cosAng, sinAng: untyped) : untyped = 
  ## Rotates p around origin, given the sin and cos of the angle of 
  ## rotation.
  (x: p.x * cosAng - p.y * sinAng, y: p.x * sinAng + p.y * cosAng)

template rotateAroundOrigin*(p: V2X; ang: untyped) : untyped = 
  ## Rotates p around origin, by angle ``ang``.
  rotateAroundOrigin(p, cos(ang), sin(ang))

template rotateAroundPoint*(p: V2X; ang: untyped; center: V2X) : untyped = 
  ## Rotate ``p`` by ``ang`` with ``center`` as the center for the rotation.
  rotateAroundOrigin(p - center, cos(ang), sin(ang)) + center

template rotateAroundPoint*(p: V2X; cosAng, sinAng: untyped; center: V2X) : untyped = 
  ## Rotate ``p`` by ``ang`` with ``center`` as the center for the rotation.
  rotateAroundOrigin(p - center, cosAng, sinAng) + center

template ff[S](x: S) : string =
    formatFloat(float(x), ffDecimal, 2)

proc `$`*[S](p: V2X[S]) : string =
    "P($1, $2)" % [ff(p.x), ff(p.y)]

proc `$`*[S](p: V3X[S]) : string =
    "P($1, $2, $3)" % [ff(p.x), ff(p.y), ff(p.z)]

proc `$`*[S](sg: Seg2X[S]) : string =
    "{$1 -> $2}" % [$sg.start, $sg.endPoint]

proc `$`*[S](m: Matrix4x4X[S]) : string =
    let args = [
        ff(m.M11), ff(m.M12), ff(m.M13), ff(m.M14),
        ff(m.M21), ff(m.M22), ff(m.M23), ff(m.M24),
        ff(m.M31), ff(m.M32), ff(m.M33), ff(m.M34),
        ff(m.M41), ff(m.M42), ff(m.M43), ff(m.M44)]

    result = "M($1 $2 $3 $4/$5 $6 $7 $8/$9 $10 $11 $12/$13 $14 $15 $16)" % args

proc `$`*[T](a: Interval[T]) : string =
    if ok(a):
        "[$1 -> $2]" % [ff(a.lo), ff(a.hi)]
    else:
        "[empty]"

proc `$`*[T](a: (Interval[T], Interval[T])) : string =
    # Downside to using tuples as points - it sometimes catches other
    # tuples. Can probably be fixed later with user typeclasses once they
    # are stable.
    result = "(" & $a[0] & ", " & $a[1] & ")"

template deriv*(f: untyped; x, dx: untyped) : untyped =
  ## Numerical derivitive of f(x) with the given DX.
  (f(x+dx) - f(x)) / dx

template newtonsMethod*(f: untyped; firstGuess: untyped; goodEnough: untyped; dx: untyped) : untyped =
  ## Newton's method.  Approximate the value of GUESS for the equation f(guess) = 0.  Returns when
  ## goodEnough(guess) returns true.
  ##  f - proc (guess: T) : T
  ## goodEnough(guess: T) : bool
  var guess = firstGuess

  while not goodEnough(guess):
    guess = guess - f(guess)/deriv(f, guess, dx)

  guess

template GB*(n: untyped) : untyped = 1024 * 1024 * 1024 * n
  ## Converts gigabytes to bytes.

template MB*(n: untyped) : untyped = 1024 * 1024 * n
  ## Converts megabytes to bytes.

template KB*(n: untyped) : untyped = 1024 * n
  ## Converts kilobytes to bytes.

proc Minutes*[T](n: T) : T = 
  ## Converts minutes to seconds.
  n * T(60)

template Hours*(n: typed) : untyped = float(n) * 60*60
  ## Converts hours to seconds.

iterator pointsOnLine*(a, b: V2X[SomeInteger]) : auto = 
  ## Iterates all points on the line from ``a`` to 
  ## ``b``, using Bresenham's algorithm.  Doesn't necessarily start at ``a``.
  let steep = abs(b.y - a.y) > abs(b.x - a.x)
  var pa = a
  var pb = b

  # Munge everything to work in single octant.
  if steep:
    swap(pa.x, pa.y)
    swap(pb.x, pb.y)

  if pa.x > pb.x:
    swap(pa.x, pb.x)
    swap(pa.y, pb.y)

  var dX = pb.x - pa.x
  var dY = abs(pb.y - pa.y)
  var err = dX div 2
  var ystep = if pa.y < pb.y: 1 else: -1
  var y = pa.y

  for x in pa.x..pb.x:
    if steep:
      yield (x: y, y: x)
    else:
      yield (x: x, y: y)

    err = err - dY
    if err < 0:
      y += ystep
      err += dX

iterator pointsOnLine*(a, b: V2X[int]) : auto = 
  ## Iterates all points on the line from ``a`` to 
  ## ``b``, using Bresenham's algorithm.  
  # Transcribed from Wikipedia page.
  let dx = abs(a.x - b.x)
  let sx = if a.x < b.x: 1 else: -1
  let dy = -abs(b.y - a.y)
  let sy = if a.y < b.y: 1 else: -1
  var err = dx+dy

  var pa = a
  while true:
    yield pa
    if pa == b:
      break

    let e2 = 2*err

    if e2 >= dy:
      err += dy
      pa.x += sx

    if e2 <= dx:
      err += dx
      pa.y += sy

template degrees*[T](deg: T) : T = 
  ## Converts degrees to the radians unit used
  ## in this and the math modules.
  deg * PI / T(180)

when isMainModule:
  import algorithm, strformat

  proc testBres() =
    var a: seq[V2i]
    const rng = 10
    for px in -rng..rng:
      for py in -rng..rng:
        let dest = (x: px, y: py)

        setLen(a, 0)
        for p in pointsOnLine((0,0), dest):
          add(a, p)

        assert a[0] == (0,0), &"ord always starts at (0,0), dest={dest}, {a}"
        assert a[^1] == dest, &"dest is always last: dest={dest}, {a}"

        for i in 1..<len(a):
          assert a[i-1] != a[i], &"No stuttering, dest={dest}, {a}"

  testBres()
