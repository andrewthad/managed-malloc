module Lib
  ( Heap
  , Pointer
  , someFunc
  ) where

data Heap s = Heap s
  { _heapRegions :: !(Regions s)
    -- ^ Array of blocks, each twice as big as the last.
    --   These blocks are allocated lazily.
  , _heapLinkedLists :: !(MutableByteArray s)
    -- ^ Pointer to first node of each tracked block size.
  , _heapMeta :: !(MutableByteArray s)
  }

newtype Pointer s = Pointer Word
newtype Regions s = Regions (MutableArrayArray# s)

data ResolvedAddr s = ResolvedAddr !Int !(MutableByteArray s)

allocate ::
     Heap s -- heap
  -> Int -- number of bytes
  -> ST s (Pointer s)
allocate h nb = do
  m <- findFreeBlock h (byteCountToBlockIndex nb)
  case m of
    Just ptr -> return ptr
    Nothing -> do
      activateNextBlock h
      allocate h nb

findFreeBlock :: Heap s -> Int -> ST s (Maybe (Pointer s))
findFreeBlock h@(regions blocks _) ix = do
  addr :: Word <- readByteArray blocks ix
  -- zero means that this area has never been assigned.
  -- one means that it has been assigned but that it is currently empty.
  case addr of
    0 -> return Nothing
    1 -> findFreeBlock h (ix + 1)
    _ -> do
      resAddr <- resolve addr regions
      status <- blockStatus resAddr
      case status of
        Vacant -> error "uhteohun" -- Figure out what to do. The block may be way too big (split it).
        Occupied -> findFreeBlock h (ix + 1)
      
resolve :: Int -> Regions s -> ST s (ResolvedAddr s ())
resolve addr (Regions _) = error "uhoetnuh"

data Status = Occupied | Vacant

blockStatus :: ResolvedAddr s -> ST s Status
blockStatus (ResolvedAddr ix marr) = do
  w :: Word <- readByteArray marr ix
  if w == 0
    then return Vacant
    else return Occupied

activateNextBlock :: Heap s -> ST s ()
activateNextBlock (Heap regions _ _) = do
  i <- firstUnusedIndex regions
  barr <- newByteArray (regionIndexToSize i)
  writeRegions regions i barr

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newHeap :: ST s (Heap s)
newHeap = Heap
  <$> newRegions
  <*> newLinkedListBlocks
  <*> newByteArray (sizeOf (undefined :: Word) * 0)

newLinkedListBlocks :: ST s (MutableByteArray s)
newLinkedListBlocks = do
  marr <- newByteArray (sizeOf (undefined :: Word) * totalLinkedLists)
  setByteArray marr 0 totalLinkedLists (0 :: Word)
  return marr
  

regionIndexToSize :: Int -> Int
regionIndexToSize i = unsafeShiftL (1 :: Int) (i + startRegion)

-- basically just log2
byteCountToBlockIndex :: Int -> Int
byteCountToBlockIndex = error "uhnoebunb"
  
newRegions :: ST s (Regions s)
newRegions = error "uhenotuh"

firstUnusedIndex :: Regions s -> ST s Int
firstUnusedIndex (Regions marr) = error "uhotneuh"

startRegion :: Int
startRegion = 12

totalRegions :: Int
totalRegions = 30

totalLinkedLists :: Int
totalLinkedLists = 30

