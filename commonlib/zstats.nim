import
  strformat

type
  Stat* = object
    numSamples: int
    hi, lo, avg, total: float

proc `$`(s: Stat) : string = 
  &"hi={s.hi:16}, lo={s.lo:16}, avg={s.avg:16}, total={s.total:16}\t\tnumSamples={s.numSamples}"

type
  ZStatName* = enum
    znTextureBytesUploadedPerFrame,
    znOtherBytesUploadedPerFrame,
    znFrameTime

  StatRegistry* = ref object
    ## Array of stats indexed by number.  All of the stats are 
    ## not known at compiletime, so the best we can do is index
    ## them by number.
    stats: array[ZStatName, Stat]

const MaxStatNum* = 50
  ## Stats are numbered, and this is the max number.

const MinUserStat* = 20
  ## Stat numbers below this are reserved for the core library.

var Stats {.threadvar.} : StatRegistry
  ## Per thread registry.

proc newStatRegistry() : StatRegistry = 
  let r = StatRegistry()
  for s in mitems(r.stats):
    s.hi = low(float)
    s.lo = high(float)

  return r

proc InitForThread*() = 
  ## Initializes the stat registry for the current thread.
  ## Can be called more than one time without harm.
  if Stats == nil:
    Stats = newStatRegistry()

proc PrintReport*() = 
  ## Prints a report on all of the stats.  
  assert Stats != nil, "call initForThread() on this thread"
  let s = Stats
  echo "STAT REPORT"
  for i in ZStatName:
    echo &"{i}\t\t{s.stats[i]}"
    
proc Record*(statNum: ZStatName; n: float) = 
  ## Records a sample of a stat.
  assert Stats != nil, "call initForThread() on this thread"
  let s = Stats.stats[statNum].addr

  let ns = s.numSamples.float
  s.avg = (n + ns*s.avg) / (ns+1.0)
  inc(s.numSamples)
  if n > s.hi:
    s.hi = n
  if n < s.lo:
    s.lo = n

  {.push checks:off.}
  s.total += n
  {.pop.}

## Central Moving Average  - CMA(n+1) = x(n+1) + n * CMA(n)
##                                      --------------------
##                                            n + 1

