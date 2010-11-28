{-# LANGUAGE CPP, BangPatterns, Rank2Types, MagicHash #-}
-- |
-- Module      : UnboxedAppend
-- Copyright   : (c) 2010 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Try using unboxed pointers for the continuation calls to make abstract
-- appends go faster.
--
module UnboxedAppend where

import Foreign
import Foreign.UPtr
import Data.Monoid
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
import Blaze.ByteString.Builder.Write (Write(..))
import qualified Blaze.ByteString.Builder.Word     as B
import Blaze.ByteString.Builder.Word (writeWord8)

import Criterion.Main

------------------------------------------------------------------------------
-- Benchmarks
------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ concat
    [ benchmark "mconcat . map fromWord8"
        myfromWord8s
        yourfromWord8s
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

myfromWord8s :: [Word8] -> Put ()
myfromWord8s = putBuilder . mconcat . map fromWord8
{-# NOINLINE myfromWord8s #-}

yourfromWord8s :: [Word8] -> B.Builder
yourfromWord8s = mconcat . map B.fromWord8
{-# NOINLINE yourfromWord8s #-}


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

type PutStep a =  UPtr -> UPtr -> IO (PutSignal a)

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

putBuilder :: Builder -> Put ()
putBuilder (Builder build) = Put $ \k -> build (k ())

fromPut :: Put () -> Builder
fromPut (Put put) = Builder $ \k -> put (\_ -> k)

fromBuildStep :: (forall r. PutStep r -> BufRange -> IO (PutSignal r)) -> Builder
fromBuildStep step = Builder step'
  where
    step' k op ope = step k (BufRange (uptrToPtr op) (uptrToPtr ope))
{-# INLINE fromBuildStep #-}

callBuildStep :: PutStep a -> BufRange -> IO (PutSignal a)
callBuildStep k (BufRange op ope) = k (ptrToUPtr op) (ptrToUPtr ope)
{-# INLINE callBuildStep #-}

boxBuildStep :: PutStep a -> (BufRange -> IO (PutSignal a))
boxBuildStep step (BufRange op ope) = step (ptrToUPtr op) (ptrToUPtr ope)
{-# INLINE boxBuildStep #-}

unboxBuildStep :: (BufRange -> IO (PutSignal a)) -> PutStep a
unboxBuildStep step op ope = step (BufRange (uptrToPtr op) (uptrToPtr ope))
{-# INLINE unboxBuildStep #-}

fromWriteSingleton :: (a -> Write) -> a -> Builder
fromWriteSingleton write = 
    mkBuilder
  where
    mkBuilder x = fromBuildStep step
      where
        step k (BufRange pf pe)
          | pf `plusPtr` size <= pe = do
              io pf
              let !br' = BufRange (pf `plusPtr` size) pe
              callBuildStep k br'
          | otherwise               = 
              return $ BufferFull size pf (unboxBuildStep $ step k)
          where
            Write size io = write x
{-# INLINE fromWriteSingleton #-}

fromWord8 :: Word8 -> Builder
fromWord8 = fromWriteSingleton writeWord8
{-# INLINE fromWord8 #-}

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
    runStep (put $ (\x -> unboxBuildStep $ finalStep x))
  where
    finalStep x !(BufRange op _) = return $ Done op x

    runStep step buf@(Buffer fpbuf p0 op ope) = do
        let !br = BufRange op ope
        signal <- liftIO $ callBuildStep step br
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
    buf0 = inlinePerformIO $ allocBuffer B.defaultBufferSize
    -- run put, but don't force result => we're lazy enough
    LBSM (result, k) = runPut liftIO outputBuf outputBS put buf0
    -- convert a buffer to a lazy bytestring continuation
    bufToLBSCont = maybe id L.Chunk . unsafeFreezeNonEmptyBuffer
    -- lifting an io putsignal to a lazy bytestring monad
    liftIO io = LBSM (inlinePerformIO io, id)
    -- add buffer as a chunk prepare allocation of new one
    outputBuf minSize buf = LBSM
        ( inlinePerformIO $ allocBuffer (max minSize B.defaultBufferSize)
        , bufToLBSCont buf )
    -- add bytestring directly as a chunk; exploits postcondition of runPut
    -- that bytestrings are non-empty
    outputBS bs = LBSM ((), L.Chunk bs)
