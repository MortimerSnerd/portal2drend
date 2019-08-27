import 
   os, strformat, winlean

var
    wfile_path = "~/.elmo/"


proc init*() = 
    #discard localtime(tt)
    #xseed(cast[uint64](tt), cast[uint64](getpid()))

    var path : array[1024, char]
    let 
      ps = cast[cstring](addr(path[0]))
      rc = 0 #shfolder removed? SHGetFolderPathA(0, CSIDL_LOCAL_APPDATA, 0, 0, ps)

    if rc == 0:
      wfile_path = $ps & "\\"

    if not existsDir(wfile_path):
       createDir(wfile_path)

proc platform_data_path*(relpath: string = "") : string = 
    #TODO: can't just assume we're in the right directory, make this an absolute path.
    "data/" & relpath

proc writable_file_path*(relpath: string = "") : string = 
    ## Returns the base path for where we can write files, with a trailing slash.
    wfile_path & relpath
    
#
# Virtual memory allocation.  Windows defines copied wholesale from system/osalloc.
#
const
  MEM_RESERVE = 0x2000
  MEM_COMMIT = 0x1000
  PAGE_READWRITE = 0x04
  PAGE_READONLY = 0x02

  MEM_DECOMMIT = 0x4000
  MEM_RELEASE = 0x8000

proc virtualAlloc(lpAddress: pointer, dwSize: int, flAllocationType,
                  flProtect: int32): pointer {.
                  header: "<windows.h>", stdcall, importc: "VirtualAlloc".}

proc virtualFree(lpAddress: pointer, dwSize: int,
                 dwFreeType: int32): cint {.header: "<windows.h>", stdcall,
                 importc: "VirtualFree".}

template chuckAWobbly(msg: typed) = 
  let e = newException(OSError, msg)

  e.errorCode = getLastError()
  raise e

proc allocVirtualMemory*[T](numItems: Positive) : ptr T = 
  ## Allocates enough virtual memory for an array of 'numItems' T
  ## and returns the pointer. The memory is guaranteed to be cleared to zero.  (TODO: prob on linux?)
  ## Will raise OSError if the allocation fails.
  result = cast[ptr T](virtualAlloc(nil, sizeof(T) * numItems.int, MEM_COMMIT or MEM_RESERVE, 
                                    PAGE_READWRITE))

  if result.isNil:
    chuckAWobbly("allocVirtualMemory failure")

proc freeVirtualMemory*[T](p: ptr T) = 
  ## Returns memory allocated by allocVirtualMemory back to the system.
  ## Can raise OSError if the free fails.
  if virtualFree(p, 0, MEM_RELEASE) == 0:
    chuckAWobbly("freeVirtualMemory failure")

#
# Memory mapped files.
#
type
  FileMapping* = object
    ## Represents an open file mapping.
    file: Handle
    mapping: Handle
    address*: pointer
    length*: int

proc NewFileMapping*(path: string) : FileMapping {.raises: [IOError,OSError].} = 
  ## Maps the given file into memory.
  const INVALID_FILE_SIZE = -1
  let fh = createFileW(newWideCString(path), 
                       GENERIC_READ, 
                       0, 
                       nil, 
                       OPEN_EXISTING, 
                       FILE_ATTRIBUTE_NORMAL, 0)

  if fh == INVALID_HANDLE_VALUE:
    raise newException(IOError, fmt"newFileMapping({path}): open failed [{getLastError()}]")

  #TODO no mapping files > 2G with error checking, use getFileSizeEx instead.
  var fileSzHi : DWORD
  let fileSzLow = getFileSize(fh, fileSzHi.addr) 
  
  if fileSzLow == INVALID_FILE_SIZE:
    discard closeHandle(fh)
    raise newException(IOError, fmt"newFileMapping({path}): getFileSize failed {getLastError()}")

  let fmap = createFileMappingW(fh, 
                                nil, 
                                PAGE_READONLY, 
                                fileSzHi, 
                                fileSzLow, 
                                nil)

  if fmap == 0:
    discard closeHandle(fh)
    raise newException(IOError, fmt"newFileMapping({path}): createFileMapping failed {getLastError()}")

  let adr = mapViewOfFileEx(fmap, 
                            FILE_MAP_READ, 
                            0, 
                            0, 
                            0, 
                            nil)
  if adr.isNil:
    discard closeHandle(fmap)
    discard closeHandle(fh)
    raise newException(IOError, fmt"newFileMapping({path}): mapViewOfFile failed {getLastError()}")

  result = FileMapping(file: fh, mapping: fmap, address: adr, length: fileSzHi.int shl 32 + fileSzLow)

proc DisposeOf*(m: var FileMapping) = 
  ## Frees file mapping.
  if not m.address.isNil:
    discard unmapViewOfFile(m.address)
    m.address = nil
    discard closeHandle(m.mapping)
    discard closeHandle(m.file)

