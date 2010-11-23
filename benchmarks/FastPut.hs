{-# LANGUAGE CPP, BangPatterns, Rank2Types #-}
-- |
-- Module      : FastPut
-- Copyright   : (c) 2010 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Implementation of a 'Put' monad with similar performance characteristics
-- like the 'Builder' monoid.
--
module FastPut where

import Foreign
import Data.Monoid
import Control.Monad (unless)
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

#ifdef BYTESTRING_IN_BASE
import Data.ByteString.Base (inlinePerformIO)
import qualified Data.ByteString.Base as S
import qualified Data.ByteString.Lazy.Base as L -- FIXME: is this the right module for access to 'Chunks'?
#else
import Data.ByteString.Internal (inlinePerformIO)
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy.Internal as L
#endif

import qualified Blaze.ByteString.Builder.Internal as B
import qualified Blaze.ByteString.Builder.Write    as B
import Blaze.ByteString.Builder.Write (Write(..))
import qualified Blaze.ByteString.Builder.Word     as B
import Blaze.ByteString.Builder.Word (writeWord8)

import Criterion.Main

------------------------------------------------------------------------------
-- Benchmarks
------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ concat
    [ return $ bench "cost of putBuilder" $ whnf
        (L.length . toLazyByteString2 . mapM_  (fromBuilder . fromWord8))
        word8s
    , benchmark "putBuilder"
        (fromBuilder . mconcat . map fromWord8)
        (mconcat . map B.fromWord8)
        word8s
    , benchmark "fromWriteSingleton"
        (mapM_ putWord8)
        (mconcat . map B.fromWord8)
        word8s
    , benchmark "fromWrite"
        (mapM_ (putWrite . writeWord8))
        (mconcat . map (B.fromWrite . writeWord8))
        word8s
    ]
  where
    benchmark name putF builderF x =
        [ bench (name ++ " Put") $
            whnf (L.length . toLazyByteString2 . putF) x
        , bench (name ++ " Builder") $
            whnf (L.length . B.toLazyByteString . builderF) x
        ]

word8s :: [Word8]
word8s = take 100000 $ cycle [0..]
{-# NOINLINE word8s #-}

------------------------------------------------------------------------------
-- The Put type
------------------------------------------------------------------------------

data BufRange = BufRange {-# UNPACK #-} !(Ptr Word8) {-# UNPACK #-} !(Ptr Word8)

newtype Put a = Put {
    unPut :: forall r. (a -> PutStep r) -> PutStep r
  }

data PutSignal a =
    Done {-# UNPACK #-} !(Ptr Word8) a
  | BufferFull
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !(Ptr Word8)
                     !(PutStep a)
  | InsertByteString
      {-# UNPACK #-} !(Ptr Word8) 
                     !S.ByteString
                     !(PutStep a)

type PutStep a =  BufRange -> IO (PutSignal a)

instance Monad Put where
  return x = Put $ \k -> k x
  {-# INLINE return #-}
  m >>= f  = Put $ \k -> unPut m (\x -> unPut (f x) k)
  {-# INLINE (>>=) #-}
  m >>  n  = Put $ \k -> unPut m (\_ -> unPut n k)
  {-# INLINE (>>) #-}

------------------------------------------------------------------------------
-- The Builder type with equal signals as the Put type
------------------------------------------------------------------------------

newtype Builder = Builder (forall r. PutStep r -> PutStep r)

instance Monoid Builder where
  mempty = Builder id
  {-# INLINE mempty #-}
  (Builder b1) `mappend` (Builder b2) = Builder $ b1 . b2
  {-# INLINE mappend #-}
  mconcat = foldr mappend mempty
  {-# INLINE mconcat #-}

fromBuilder :: Builder -> Put ()
fromBuilder (Builder build) = Put $ \k -> build (k ())

toBuilder :: Put () -> Builder
toBuilder (Put put) = Builder $ \k -> put (\_ -> k)

fromWrite :: Write -> Builder
fromWrite (Write size io) =
    Builder step
  where
    step k (BufRange pf pe)
      | pf `plusPtr` size <= pe = do
          io pf
          let !br' = BufRange (pf `plusPtr` size) pe
          k br'
      | otherwise = return $ BufferFull size pf (step k)
{-# INLINE fromWrite #-}

fromWriteSingleton :: (a -> Write) -> a -> Builder
fromWriteSingleton write = 
    mkPut
  where
    mkPut x = Builder step
      where
        step k (BufRange pf pe)
          | pf `plusPtr` size <= pe = do
              io pf
              let !br' = BufRange (pf `plusPtr` size) pe
              k br'
          | otherwise               = return $ BufferFull size pf (step k)
          where
            Write size io = write x
{-# INLINE fromWriteSingleton #-}

fromWord8 :: Word8 -> Builder
fromWord8 = fromWriteSingleton writeWord8


------------------------------------------------------------------------------
-- Implementations
------------------------------------------------------------------------------

putWord8 :: Word8 -> Put ()
putWord8 = putWriteSingleton writeWord8

putWrite :: Write -> Put ()
putWrite (Write size io) =
    Put step
  where
    step k (BufRange pf pe)
      | pf `plusPtr` size <= pe = do
          io pf
          let !br' = BufRange (pf `plusPtr` size) pe
          k () br'
      | otherwise = return $ BufferFull size pf (step k)
{-# INLINE putWrite #-}

putWriteSingleton :: (a -> Write) -> a -> Put ()
putWriteSingleton write = 
    mkPut
  where
    mkPut x = Put step
      where
        step k (BufRange pf pe)
          | pf `plusPtr` size <= pe = do
              io pf
              let !br' = BufRange (pf `plusPtr` size) pe
              k () br'
          | otherwise               = return $ BufferFull size pf (step k)
          where
            Write size io = write x
{-# INLINE putWriteSingleton #-}

putBuilder :: B.Builder -> Put ()
putBuilder (B.Builder b) = 
    Put step
  where
    finalStep _ pf = return $ B.Done pf

    step k = go (b finalStep)
      where
        go buildStep (BufRange pf pe) = do
          signal <- buildStep pf pe
          case signal of
            B.Done pf' -> do
              let !br' = BufRange pf' pe
              k () br'
            B.BufferFull minSize pf' nextBuildStep -> 
              return $ BufferFull minSize pf' (go nextBuildStep)
            B.ModifyChunks _ _ _ -> 
              error "putBuilder: ModifyChunks not implemented"

{-
  m >>= f  = GetC $ \done empty pe ->
      runGetC m (\pr' x -> runGetC (f x) done empty pe pr') 
                (\m' -> empty (m' >>= f))
                pe


newtype GetC r a = GetC {
    runGetC ::
      (Ptr Word8 -> a -> IO r) ->   -- done
      (GetC r a -> IO r     )  ->   -- empty buffer
      Ptr Word8                ->   -- end of buffer
      Ptr Word8                ->   -- next byte to read
      IO r
  }

instance Functor (GetC r) where
  fmap f g = GetC $ \done empty ->
      runGetC g (\pr' x -> done pr' (f x)) 
                (\g'    -> empty (fmap f g'))

instance Monad (GetC r) where
  return x = GetC $ \done _ _ pr -> done pr x
  m >>= f  = GetC $ \done empty pe ->
      runGetC m (\pr' x -> runGetC (f x) done empty pe pr') 
                (\m' -> empty (m' >>= f))
                pe

-}

------------------------------------------------------------------------------
-- Internal global constants.
------------------------------------------------------------------------------

-- | Default size (~32kb) for the buffer that becomes a chunk of the output
-- stream once it is filled.
--
defaultBufferSize :: Int
defaultBufferSize = 32 * 1024 - overhead -- Copied from Data.ByteString.Lazy.
    where overhead = 2 * sizeOf (undefined :: Int)

-- | The minimal length (~4kb) a buffer must have before filling it and
-- outputting it as a chunk of the output stream. 
--
-- This size determines when a buffer is spilled after a 'flush' or a direct
-- bytestring insertion. It is also the size of the first chunk generated by
-- 'toLazyByteString'.
defaultMinimalBufferSize :: Int
defaultMinimalBufferSize = 4 * 1024 - overhead
    where overhead = 2 * sizeOf (undefined :: Int)

-- | The default length (64) for the first buffer to be allocated when
-- converting a 'Builder' to a lazy bytestring. 
--
-- See 'toLazyByteStringWith' for further explanation.
defaultFirstBufferSize :: Int
defaultFirstBufferSize = 64

-- | The maximal number of bytes for that copying is cheaper than direct
-- insertion into the output stream. This takes into account the fragmentation
-- that may occur in the output buffer due to the early 'flush' implied by the
-- direct bytestring insertion.
--
-- @'defaultMaximalCopySize' = 2 * 'defaultMinimalBufferSize'@
--
defaultMaximalCopySize :: Int
defaultMaximalCopySize = 2 * defaultMinimalBufferSize

------------------------------------------------------------------------------
-- Flushing and running a Builder
------------------------------------------------------------------------------


-- | Output all data written in the current buffer and start a new chunk.
--
-- The use uf this function depends on how the resulting bytestrings are
-- consumed. 'flush' is possibly not very useful in non-interactive scenarios.
-- However, it is kept for compatibility with the builder provided by
-- Data.Binary.Builder.
--
-- When using 'toLazyByteString' to extract a lazy 'L.ByteString' from a
-- 'Builder', this means that a new chunk will be started in the resulting lazy
-- 'L.ByteString'. The remaining part of the buffer is spilled, if the
-- reamining free space is smaller than the minimal desired buffer size.
--
{-
flush :: Builder
flush = Builder $ \k pf _ -> return $ ModifyChunks pf id k
-}

-- | Run a 'Builder' with the given buffer sizes.
--
-- Use this function for integrating the 'Builder' type with other libraries
-- that generate lazy bytestrings.
--
-- Note that the builders should guarantee that on average the desired chunk
-- size is attained. Builders may decide to start a new buffer and not
-- completely fill the existing buffer, if this is faster. However, they should
-- not spill too much of the buffer, if they cannot compensate for it.
--
-- A call @toLazyByteStringWith bufSize minBufSize firstBufSize@ will generate
-- a lazy bytestring according to the following strategy. First, we allocate
-- a buffer of size @firstBufSize@ and start filling it. If it overflows, we
-- allocate a buffer of size @minBufSize@ and copy the first buffer to it in
-- order to avoid generating a too small chunk. Finally, every next buffer will
-- be of size @bufSize@. This, slow startup strategy is required to achieve
-- good speed for short (<200 bytes) resulting bytestrings, as for them the
-- allocation cost is of a large buffer cannot be compensated. Moreover, this
-- strategy also allows us to avoid spilling too much memory for short
-- resulting bytestrings.
--
-- Note that setting @firstBufSize >= minBufSize@ implies that the first buffer
-- is no longer copied but allocated and filled directly. Hence, setting
-- @firstBufSize = bufSize@ means that all chunks will use an underlying buffer
-- of size @bufSize@. This is recommended, if you know that you always output
-- more than @minBufSize@ bytes.
toLazyByteStringWith 
    :: Int           -- ^ Buffer size (upper-bounds the resulting chunk size).
    -> Int           -- ^ Minimal free buffer space for continuing filling
                     -- the same buffer after a 'flush' or a direct bytestring
                     -- insertion. This corresponds to the minimal desired
                     -- chunk size.
    -> Int           -- ^ Size of the first buffer to be used and copied for
                     -- larger resulting sequences
    -> Put a       -- ^ Builder to run.
    -> L.ByteString  -- ^ Lazy bytestring to output after the builder is
                     -- finished.
    -> L.ByteString  -- ^ Resulting lazy bytestring
toLazyByteStringWith bufSize minBufSize firstBufSize (Put b) k = 
    inlinePerformIO $ fillFirstBuffer (b finalStep)
  where
    finalStep _ (BufRange pf _) = return $ Done pf undefined
    -- fill a first very small buffer, if we need more space then copy it
    -- to the new buffer of size 'minBufSize'. This way we don't pay the
    -- allocation cost of the big 'bufSize' buffer, when outputting only
    -- small sequences.
    fillFirstBuffer !step0
      | minBufSize <= firstBufSize = fillNewBuffer firstBufSize step0
      | otherwise                  = do
          fpbuf <- S.mallocByteString firstBufSize
          withForeignPtr fpbuf $ \pf -> do
              let !br      = BufRange pf (pf `plusPtr` firstBufSize)
                  mkbs pf' = S.PS fpbuf 0 (pf' `minusPtr` pf)
                  {-# INLINE mkbs #-}
              next <- step0 br
              case next of
                  Done pf' _
                    | pf' == pf -> return k
                    | otherwise -> return $ L.Chunk (mkbs pf') k

                  BufferFull newSize pf' nextStep  -> do
                      let !l  = pf' `minusPtr` pf
                      fillNewBuffer (max (l + newSize) minBufSize) $
                          \(BufRange pfNew peNew) -> do
                              copyBytes pfNew pf l
                              let !brNew = BufRange (pfNew `plusPtr` l) peNew
                              nextStep brNew
                      
                  InsertByteString _ _ _ -> error "not yet implemented"
                  {-
                  ModifyChunks pf' bsk nextStep( 
                      | pf' == pf ->
                          return $ bsk (inlinePerformIO $ fillNewBuffer bufSize nextStep)
                      | otherwise ->
                          return $ L.Chunk (mkbs pf')
                              (bsk (inlinePerformIO $ fillNewBuffer bufSize nextStep))
                  -}
                    
    -- allocate and fill a new buffer
    fillNewBuffer !size !step0 = do
        fpbuf <- S.mallocByteString size
        withForeignPtr fpbuf $ fillBuffer fpbuf
      where
        fillBuffer fpbuf !pbuf = fill pbuf step0
          where
            !pe = pbuf `plusPtr` size
            fill !pf !step = do
                let !br = BufRange pf pe
                next <- step br
                let mkbs pf' = S.PS fpbuf (pf `minusPtr` pbuf) (pf' `minusPtr` pf)
                    {-# INLINE mkbs #-}
                case next of
                    Done pf' _
                      | pf' == pf -> return k
                      | otherwise -> return $ L.Chunk (mkbs pf') k

                    BufferFull newSize pf' nextStep ->
                        return $ L.Chunk (mkbs pf')
                            (inlinePerformIO $ 
                                fillNewBuffer (max newSize bufSize) nextStep)
                        
                    InsertByteString _ _ _ -> error "not yet implemented2"
                    {-
                    ModifyChunks  pf' bsk nextStep
                      | pf' == pf                      ->
                          return $ bsk (inlinePerformIO $ fill pf' nextStep)
                      | minBufSize < pe `minusPtr` pf' ->
                          return $ L.Chunk (mkbs pf')
                              (bsk (inlinePerformIO $ fill pf' nextStep))
                      | otherwise                      ->
                          return $ L.Chunk (mkbs pf')
                              (bsk (inlinePerformIO $ fillNewBuffer bufSize nextStep))
                    -}


-- | Extract the lazy 'L.ByteString' from the builder by running it with default
-- buffer sizes. Use this function, if you do not have any special
-- considerations with respect to buffer sizes.
--
-- @ 'toLazyByteString' b = 'toLazyByteStringWith' 'defaultBufferSize' 'defaultMinimalBufferSize' 'defaultFirstBufferSize' b L.empty@
--
-- Note that @'toLazyByteString'@ is a 'Monoid' homomorphism.
--
-- > toLazyByteString mempty          == mempty
-- > toLazyByteString (x `mappend` y) == toLazyByteString x `mappend` toLazyByteString y
--
-- However, in the second equation, the left-hand-side is generally faster to
-- execute.
--
toLazyByteString :: Put a -> L.ByteString
toLazyByteString b = toLazyByteStringWith 
    defaultBufferSize defaultMinimalBufferSize defaultFirstBufferSize b L.empty
{-# INLINE toLazyByteString #-}

------------------------------------------------------------------------------
-- Builder Enumeration
------------------------------------------------------------------------------

data BuildStream a = 
         BuildChunk  S.ByteString (IO (BuildStream a))
       | BuildYield
           a            
           (forall b. Bool -> 
                      Either (Maybe S.ByteString) (Put b -> IO (BuildStream b)))

enumPut :: Int -> Put a -> IO (BuildStream a)
enumPut bufSize (Put put0) =
    fillBuffer bufSize (put0 finalStep)
  where
    finalStep :: forall b. b -> PutStep b
    finalStep x (BufRange op _) = return $ Done op x

    fillBuffer :: forall b. Int -> PutStep b -> IO (BuildStream b)
    fillBuffer size step = do
        fpbuf <- S.mallocByteString bufSize 
        let !pbuf = unsafeForeignPtrToPtr fpbuf
                  -- safe due to later reference of fpbuf
                  -- BETTER than withForeignPtr, as we lose a tail call otherwise
            !br = BufRange pbuf (pbuf `plusPtr` size)
        fillStep fpbuf br step

    fillPut :: ForeignPtr Word8 -> BufRange -> 
               Bool -> Either (Maybe S.ByteString) (Put b -> IO (BuildStream b))
    fillPut !fpbuf !(BufRange op _) False 
      | pbuf == op = Left Nothing
      | otherwise  = Left $ Just $
          S.PS fpbuf 0 (op `minusPtr` pbuf)
      where
        pbuf = unsafeForeignPtrToPtr fpbuf
        {-# INLINE pbuf #-}
    fillPut !fpbuf !br True =
        Right $ \(Put put) -> fillStep fpbuf br (put finalStep)

    fillStep :: forall b. ForeignPtr Word8 -> BufRange -> PutStep b -> IO (BuildStream b)
    fillStep !fpbuf !br@(BufRange _ ope) step = do
        let pbuf = unsafeForeignPtrToPtr fpbuf
            {-# INLINE pbuf #-}
        signal <- step br
        case signal of
            Done op' x -> do      -- builder completed, buffer partially filled
                let !br' = BufRange op' ope
                return $ BuildYield x (fillPut fpbuf br')

            BufferFull minSize op' nextStep  
              | pbuf == op' -> do -- nothing written, larger buffer required
                  fillBuffer (max bufSize minSize) nextStep
              | otherwise   -> do -- some bytes written, new buffer required
                  return $ BuildChunk 
                    (S.PS fpbuf 0 (op' `minusPtr` pbuf))
                    (fillBuffer (max bufSize minSize) nextStep)

            InsertByteString op' bs nextStep
              | S.null bs -> do   -- empty bytestrings are ignored
                  let !br' = BufRange op' ope
                  fillStep fpbuf br' nextStep
              | pbuf == op' -> do -- no bytes written: just insert bytestring
                  return $ BuildChunk bs (fillBuffer bufSize nextStep)
              | otherwise -> do   -- bytes written, insert buffer and bytestring
                  return $ BuildChunk (S.PS fpbuf 0 (op' `minusPtr` pbuf))
                    (return $ BuildChunk bs (fillBuffer bufSize nextStep))


toLazyByteString' :: Put () -> L.ByteString
toLazyByteString' put = 
    inlinePerformIO (consume `fmap` enumPut defaultBufferSize put)
  where
    consume :: BuildStream () -> L.ByteString
    consume (BuildYield _ f) = 
        case f False of
          Left Nothing   -> L.Empty
          Left (Just bs) -> L.Chunk bs L.Empty
          Right _        -> error "toLazyByteString': enumPut violated postcondition"
    consume (BuildChunk bs ioStream) =
        L.Chunk bs $ inlinePerformIO (consume `fmap` ioStream)
        


{-
                    BufferFull minSize pf' nextStep  -> do
                        io $ S.PS fpbuf 0 (pf' `minusPtr` pf)
                        fillBuffer (max bufSize minSize) nextStep
                        
                    ModifyChunks pf' bsk nextStep  -> do
                        io $ S.PS fpbuf 0 (pf' `minusPtr` pf)
                        L.foldrChunks (\bs -> (io bs >>)) (return ()) (bsk L.empty)
                        fillBuffer bufSize nextStep
-}

------------------------------------------------------------------------------
-- More explicit implementation of running builders
------------------------------------------------------------------------------


data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8) -- underlying pinned array
                     {-# UNPACK #-} !(Ptr Word8)        -- beginning of slice
                     {-# UNPACK #-} !(Ptr Word8)        -- next free byte
                     {-# UNPACK #-} !(Ptr Word8)        -- first byte after buffer

allocBuffer :: Int -> IO Buffer
allocBuffer size = do
    fpbuf <- S.mallocByteString size
    let !pbuf = unsafeForeignPtrToPtr fpbuf
    return $! Buffer fpbuf pbuf pbuf (pbuf `plusPtr` size)

unsafeFreezeBuffer :: Buffer -> S.ByteString
unsafeFreezeBuffer (Buffer fpbuf p0 op _) = 
    S.PS fpbuf 0 (op `minusPtr` p0)

unsafeFreezeNonEmptyBuffer :: Buffer -> Maybe S.ByteString
unsafeFreezeNonEmptyBuffer (Buffer fpbuf p0 op _) 
  | p0 == op  = Nothing
  | otherwise = Just $ S.PS fpbuf 0 (op `minusPtr` p0)

nextSlice :: Int -> Buffer -> Maybe Buffer
nextSlice minSize (Buffer fpbuf _ op ope)
  | ope `minusPtr` op <= minSize = Nothing
  | otherwise                    = Just (Buffer fpbuf op op ope)

runPut :: Monad m 
       => (IO (PutSignal a) -> m (PutSignal a)) -- lifting of buildsteps
       -> (Int -> Buffer -> m Buffer) -- output function for a guaranteedly non-empty buffer, the returned buffer will be filled next
       -> (S.ByteString -> m ())    -- output function for guaranteedly non-empty bytestrings, that are inserted directly into the stream
       -> Put a                     -- put to execute
       -> Buffer                    -- initial buffer to be used
       -> m (a, Buffer)             -- result of put and remaining buffer
runPut liftIO outputBuf outputBS (Put put) =
    runStep (put finalStep)
  where
    finalStep x !(BufRange op _) = return $ Done op x

    runStep step buf@(Buffer fpbuf p0 op ope) = do
        let !br = BufRange op ope
        signal <- liftIO $ step br
        case signal of 
            Done op' x ->         -- put completed, buffer partially runSteped
                return (x, Buffer fpbuf p0 op' ope)

            BufferFull minSize op' nextStep -> do
                buf' <- outputBuf minSize (Buffer fpbuf p0 op' ope)
                runStep nextStep buf'

            InsertByteString op' bs nextStep
              | S.null bs ->   -- flushing of buffer required
                  outputBuf 1 (Buffer fpbuf p0 op' ope) >>= runStep nextStep
              | p0 == op' -> do -- no bytes written: just insert bytestring
                  outputBS bs
                  runStep nextStep buf
              | otherwise -> do   -- bytes written, insert buffer and bytestring
                  buf' <- outputBuf 1 (Buffer fpbuf p0 op' ope)
                  outputBS bs
                  runStep nextStep buf'
{-# INLINE runPut #-}
              
-- | A monad for lazily composing lazy bytestrings using continuations.
newtype LBSM a = LBSM { unLBSM :: (a, L.ByteString -> L.ByteString) }

instance Monad LBSM where
    return x                       = LBSM (x, id)
    (LBSM (x,k)) >>= f             = let LBSM (x',k') = f x in LBSM (x', k . k')
    (LBSM (_,k)) >> (LBSM (x',k')) = LBSM (x', k . k')

-- | Execute a put and return the written buffers as the chunks of a lazy
-- bytestring.
toLazyByteString2 :: Put a -> L.ByteString
toLazyByteString2 put = 
    k (bufToLBSCont (snd result) L.empty)
  where
    -- initial buffer
    buf0 = inlinePerformIO $ allocBuffer defaultBufferSize
    -- run put, but don't force result => we're lazy enough
    LBSM (result, k) = runPut liftIO outputBuf outputBS put buf0
    -- convert a buffer to a lazy bytestring continuation
    bufToLBSCont = maybe id L.Chunk . unsafeFreezeNonEmptyBuffer
    -- lifting an io putsignal to a lazy bytestring monad
    liftIO io = LBSM (inlinePerformIO io, id)
    -- add buffer as a chunk prepare allocation of new one
    outputBuf minSize buf = LBSM
        ( inlinePerformIO $ allocBuffer (max minSize defaultBufferSize)
        , bufToLBSCont buf )
    -- add bytestring directly as a chunk; exploits postcondition of runPut
    -- that bytestrings are non-empty
    outputBS bs = LBSM ((), L.Chunk bs)

-- | A Builder that traces a message
traceBuilder :: String -> Builder 
traceBuilder msg = Builder $ \k br@(BufRange op ope) -> do
    putStrLn $ "traceBuilder " ++ show (op, ope) ++ ": " ++ msg
    k br

flushBuilder :: Builder
flushBuilder = Builder $ \k (BufRange op _) -> do
    return $ InsertByteString op S.empty k

test2 :: Word8 -> [S.ByteString]
test2 x = L.toChunks $ toLazyByteString2 $ fromBuilder $ mconcat
  [ traceBuilder "before flush" 
  , fromWord8 48
  , flushBuilder
  , flushBuilder
  , traceBuilder "after flush"
  , fromWord8 x
  ]

