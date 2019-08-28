import
  geom, glsupport, opengl

type
  VertBatch*[V,I] = ref object
    ## Batch of geometry in indexed primitive format.
    vertices*: seq[V]
    indices*: seq[I]
    primitive*: GLenum  ## GL_TRIANGLES, GL_LINES, etc

proc glIndexType[I]() : GLEnum = 
  when I is uint16:
    GL_UNSIGNED_SHORT
  else:
    {.error "Need new typecase for VertIndex.".}

proc Clear*[V,I](vb: VertBatch[V,I]) = 
  ## Clears out the vertex and index arrays.
  vb.vertices.setLen(0)
  vb.indices.setLen(0)

template HasVerts*[V,I](vb: VertBatch[V,I]) : untyped = len(vb.vertices) > 0

proc NewVertBatch*[V,I](prim: GLenum) : VertBatch[V,I] = 
  ## Creates a new vertice batch that can be submitted to the sprite
  ## renderer to be submitted at a particular depth.  Prefer larger
  ## batches, as multiple VertBatch objects do not get coalesced into 
  ## a single upload.
  result = VertBatch[V,I](primitive: prim, 
                          vertices: newSeq[V](64), 
                          indices: newSeq[I](64))
  Clear(result)

proc SubmitAndDraw*[V,I](vb: VertBatch[V,I]; vertBuf, idxBuf: BufferObject) = 
  ## Uploads the batch to the given buffer objects, and then draws it.
  ## This assumes the buffer objects are already enabled and bound.
  if len(vb.indices) > 0:
    Populate(vertBuf, GL_ARRAY_BUFFER, vb.vertices, GL_DYNAMIC_DRAW)
    Populate(idxBuf, GL_ELEMENT_ARRAY_BUFFER, vb.indices, GL_DYNAMIC_DRAW)
    glDrawElements(vb.primitive, len(vb.indices).GLsizei, glIndexType[I](), cast[pointer](0))

proc Triangulate*[V,I](vb: VertBatch[V,I]; verts: openarray[V]) = 
  ## Given a clockwise array of points for a convex polygon, triangulates
  ## the polygon. No checking is done to verify the polygon is convex.
  assert len(verts) > 2
  let base = len(vb.vertices).I
  let ibase = len(vb.indices).I
  vb.vertices.add(verts)
  vb.indices.setLen(len(vb.indices) + (len(verts) - 2) * 3)
  var
    i0 = base + 2
    i1 = base + 1
    ii = ibase.int

  let vlen = len(vb.vertices).I
  while i0 < vlen:
    vb.indices[ii] = i0
    vb.indices[ii+1] = i1
    vb.indices[ii+2] = base
    ii += 3
    i0 += 1
    i1 += 1
     
proc AddLine*[V,I](vb: VertBatch[V,I]; a, b: V) = 
  ## Adds a line to the batch to be drawn.
  assert vb.primitive == GL_LINES
  let base = len(vb.vertices).I
  
  vb.vertices.add(a)
  vb.vertices.add(b)
  vb.indices.add(base)
  vb.indices.add(base+1)

proc AddLines*[V,I,T](vb: VertBatch[V,I]; 
                      bb: AABB2X[T];
                      cvt: proc (v: V2X[T]; num: int) : V) = 
  ## Adds lines around the bounding box.
  var vtxs : array[4,V]
  let 
    base = len(vb.vertices).I
    br   = bb.bottomRight
  
  vtxs[0] = cvt(bb.topLeft, 0)
  vtxs[1] = cvt((br.x, bb.topLeft.y), 1)
  vtxs[2] = cvt(br, 2)
  vtxs[3] = cvt((bb.topLeft.x, br.y), 3)
  bb.vertices.add(vtxs)
  bb.indices.add([base, base + 1, base + 1, base + 2, base + 2, base + 3, base + 3, base])

