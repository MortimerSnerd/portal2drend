import 
   os, posix, strformat, times

var
    wfile_path = "~/.elmo/"

proc init*() = 
    #TODO: fix this, there's SDL calls we can use.
    let hm = getEnv("HOME")
    if len(hm) > 0:
        wfile_path = hm & "/.yub/"

    if not existsDir(wfile_path):
        createDir(wfile_path)

proc install_data_path*(relpath: string) : string = 
    ## Data directory of the install - not usually writable, though things like the
    ## map editor may write to it.
    "data/" & relpath

proc platform_data_path*(relpath: string = "") : string = 
    #TODO: can't just assume we're in the right directory, make this an absolute path.
    "data/" & relpath

proc writable_file_path*(relpath: string = "") : string = 
    ## Returns the base path for where we can write files, with a trailing slash.
    wfile_path & relpath
    
template chuckAWobbly(msg: typed) = 
  let e = newException(OSError, msg)

  e.errorCode = errno
  raise e

proc allocVirtualMemory*[T](numItems: Positive) : ptr T = 
  ## Allocates enough virtual memory for an array of 'numItems' T
  ## and returns the pointer. The memory is guaranteed to be cleared to zero. 
  ## Will raise OSError if the allocation fails.
  const MAP_ANON = 0x20
  let 
    sz = sizeof(T) * numItems.int + sizeof(int)
    blkPtr = cast[ptr T](mmap(nil, sz, PROT_READ or PROT_WRITE, 
                         MAP_PRIVATE or MAP_ANON, -1, 0))
    szHeader : ptr int = cast[ptr int](blkPtr)

  if blkPtr.isNil:
    chuckAWobbly("allocVirtualMemory failure")

  # Save our size so we can correctly munmap later.
  szHeader[] = sz
  result = cast[ptr T](cast[int](blkPtr) + sizeof(int))


proc freeVirtualMemory*[T](p: ptr T) = 
  ## Returns memory allocated by allocVirtualMemory back to the system.
  ## Can raise OSError if the free fails.
  let 
    szHeader = cast[ptr int](cast[int](p) - sizeof(int))
    sz = szHeader[]

  if munmap(szHeader, sz) != 0:
    chuckAWobbly("freeVirtualMemory failure")

#
# Memory mapped files.
#
type
  FileMapping* = object
    ## Represents an open file mapping.
    fd: cint
    address*: pointer
    length*: int

proc NewFileMapping*(path: string) : FileMapping {.raises: [IOError,ValueError].} = 
  ## Maps the given file into memory.
  let h = posix.open(path.cstring, O_RDONLY)
  
  if h == -1:
    raise newException(IOError, fmt"createFileMapping({path}): Can not open file [{errno}]")

  var st : posix.Stat

  if fstat(h, st) != 0:
    raise newException(IOError, fmt"createFileMapping({path}): Can not stat [{errno}].")

  let ad = mmap(nil, st.st_size, PROT_READ, MAP_PRIVATE, h, 0)

  if cast[int](ad) == -1:
    raise newException(IOError, fmt"createFileMapping({path}): map failure [{errno}]")

  return FileMapping(fd: h, address: ad, length: st.st_size.int)

proc DisposeOf*(fm: var FileMapping) {.raises: [OSError].} = 
  ## Frees a file mapping.  
  if fm.fd != 0:
    if munmap(fm.address, fm.length.cint) != 0:
      raise newException(OSError, "disposeOf(FileMapping): munmap failed [{errno}]")

    discard posix.close(fm.fd)
    fm.fd = 0
