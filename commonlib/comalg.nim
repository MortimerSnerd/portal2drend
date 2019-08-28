## Place for common, generic algorithms not supplied by stdlib.

proc linearIndex*[K, T, V](a: openarray[(T, V)]; k: K) : int = 
  ## Linear lookup for (key, value) pairs. Does not assume keys are sorted.
  for i in 0..<len(a):
    if a[i][0] == k:
      return i
  return -1

proc linear*[K, T, V](a: openarray[(T, V)]; k: K; default: V) : V = 
  ## Linear lookup for (key, value) pairs. Assumes keys are sorted ascending. Returns default if not found.
  for i in 0..<len(a):
    if a[i][0] == k:
      return a[i][1]
    if k < a[i][0]:
      return default
  return default

proc linear*[K, T, V](a: openarray[(T, V)]; k: K) : V = 
  ## Linear lookup for (key, value) pairs. Assumes keys are sorted ascending.
  ## Craps pants if not found.
  for i in 0..<len(a):
    if a[i][0] == k:
      return a[i][1]
    if k < a[i][0]:
      break
  assert false, &"No entry for {k}"

proc checkArray*[T](a: T) = 
  ## Asserts if the array is not ordered by the (key, )
  ## field of the tuple.  Intended to be used with ``linear`` 
  ## and ``binary`` search arrays, to verify the order is
  ## correct.
  var v = -1
  for i in 0..<len(a):
    let o = ord(a[i][0])
    assert o > v, &"Array is ordered, {o} > {v}"
    v = o
