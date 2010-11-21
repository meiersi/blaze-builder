{-# LANGUAGE CPP, BangPatterns #-}
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
import Blaze.ByteString.Builder.Write
import Blaze.ByteString.Builder.Word

import Criterion.Main

------------------------------------------------------------------------------
-- Benchmarks
------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ concat
    [ benchmark "putBuilder"
        (putBuilder . mconcat . map fromWord8)
        (mconcat . map fromWord8)
        word8s
    , benchmark "fromWriteSingleton"
        (mapM_ putWord8)
        (mconcat . map fromWord8)
        word8s
    , benchmark "fromWrite"
        (mapM_ (putWrite . writeWord8))
        (mconcat . map (fromWrite . writeWord8))
        word8s
    ]
  where
    benchmark name putF builderF x =
        [ bench (name ++ " Put") $
            whnf (L.length . toLazyByteString . putF) x
        , bench (name ++ " Builder") $
            whnf (L.length . B.toLazyByteString . builderF) x
        ]

word8s :: [Word8]
word8s = take 100000 $ cycle [0..]
{-# NOINLINE word8s #-}


------------------------------------------------------------------------------
-- The Builder type
------------------------------------------------------------------------------

newtype Put r a = Put {
    unPut :: (a -> Ptr Word8 -> Ptr Word8 -> IO (PutSignal r))
          -> (Ptr Word8 -> Ptr Word8 -> IO (PutSignal r))
  }

data PutSignal a =
    Done {-# UNPACK #-} !(Ptr Word8) a
  | BufferFull
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !(Ptr Word8)
                     !(PutStep a)
  | ModifyChunks
      {-# UNPACK #-} !(Ptr Word8) 
                     !(L.ByteString -> L.ByteString) 
                     !(PutStep a)

type PutStep a =  Ptr Word8      
               -> Ptr Word8   
               -> IO (PutSignal a)

instance Monad (Put r) where
  return x = Put $ \k -> k x
  {-# INLINE return #-}
  m >>= f  = Put $ \k -> unPut m (\x -> unPut (f x) k)
  {-# INLINE (>>=) #-}
  m >>  n  = Put $ \k -> unPut m (\_ -> unPut n k)
  {-# INLINE (>>) #-}

putWrite :: Write -> Put r ()
putWrite (Write size io) =
    Put step
  where
    step k !pf !pe
      | pf `plusPtr` size <= pe = do
          io pf
          let !pf' = pf `plusPtr` size
          k () pf' pe
      | otherwise = return $ BufferFull size pf (step k)
{-# INLINE putWrite #-}

putWriteSingleton :: (a -> Write) -> a -> Put r ()
putWriteSingleton write = 
    mkPut
  where
    mkPut x = Put step
      where
        step k pf pe
          | pf `plusPtr` size <= pe = do
              io pf
              let !pf' = pf `plusPtr` size
              k () pf' pe
          | otherwise               = return $ BufferFull size pf (step k)
          where
            Write size io = write x
{-# INLINE putWriteSingleton #-}

putBuilder :: B.Builder -> Put r ()
putBuilder (B.Builder b) = 
    Put step
  where
    finalStep _ pf = return $ B.Done pf

    step k = go (b finalStep)
      where
        go buildStep pf pe = do
          signal <- buildStep pf pe
          case signal of
            B.Done pf' -> 
              k () pf' pe
            B.BufferFull minSize pf' nextBuildStep -> 
              return $ BufferFull minSize pf' (go nextBuildStep)
            B.ModifyChunks _ _ _ -> 
              error "putBuilder: ModifyChunks not implemented"

putWord8 :: Word8 -> Put r ()
putWord8 = putWriteSingleton writeWord8

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
    -> Put r a       -- ^ Builder to run.
    -> L.ByteString  -- ^ Lazy bytestring to output after the builder is
                     -- finished.
    -> L.ByteString  -- ^ Resulting lazy bytestring
toLazyByteStringWith bufSize minBufSize firstBufSize (Put b) k = 
    inlinePerformIO $ fillFirstBuffer (b finalStep)
  where
    finalStep _ pf _ = return $ Done pf undefined
    -- fill a first very small buffer, if we need more space then copy it
    -- to the new buffer of size 'minBufSize'. This way we don't pay the
    -- allocation cost of the big 'bufSize' buffer, when outputting only
    -- small sequences.
    fillFirstBuffer !step0
      | minBufSize <= firstBufSize = fillNewBuffer firstBufSize step0
      | otherwise                  = do
          fpbuf <- S.mallocByteString firstBufSize
          withForeignPtr fpbuf $ \pf -> do
              let !pe      = pf `plusPtr` firstBufSize
                  mkbs pf' = S.PS fpbuf 0 (pf' `minusPtr` pf)
                  {-# INLINE mkbs #-}
              next <- step0 pf pe
              case next of
                  Done pf' _
                    | pf' == pf -> return k
                    | otherwise -> return $ L.Chunk (mkbs pf') k

                  BufferFull newSize pf' nextStep  -> do
                      let !l  = pf' `minusPtr` pf
                      fillNewBuffer (max (l + newSize) minBufSize) $
                          \pfNew peNew -> do
                              copyBytes pfNew pf l
                              nextStep (pfNew `plusPtr` l) peNew
                      
                  ModifyChunks pf' bsk nextStep 
                      | pf' == pf ->
                          return $ bsk (inlinePerformIO $ fillNewBuffer bufSize nextStep)
                      | otherwise ->
                          return $ L.Chunk (mkbs pf')
                              (bsk (inlinePerformIO $ fillNewBuffer bufSize nextStep))
                    
    -- allocate and fill a new buffer
    fillNewBuffer !size !step0 = do
        fpbuf <- S.mallocByteString size
        withForeignPtr fpbuf $ fillBuffer fpbuf
      where
        fillBuffer fpbuf !pbuf = fill pbuf step0
          where
            !pe = pbuf `plusPtr` size
            fill !pf !step = do
                next <- step pf pe
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
                        
                    ModifyChunks  pf' bsk nextStep
                      | pf' == pf                      ->
                          return $ bsk (inlinePerformIO $ fill pf' nextStep)
                      | minBufSize < pe `minusPtr` pf' ->
                          return $ L.Chunk (mkbs pf')
                              (bsk (inlinePerformIO $ fill pf' nextStep))
                      | otherwise                      ->
                          return $ L.Chunk (mkbs pf')
                              (bsk (inlinePerformIO $ fillNewBuffer bufSize nextStep))


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
toLazyByteString :: Put r a -> L.ByteString
toLazyByteString b = toLazyByteStringWith 
    defaultBufferSize defaultMinimalBufferSize defaultFirstBufferSize b L.empty
{-# INLINE toLazyByteString #-}

{-
-- | Pack the chunks of a lazy bytestring into a single strict bytestring.
packChunks :: L.ByteString -> S.ByteString
packChunks lbs = do
    S.unsafeCreate (fromIntegral $ L.length lbs) (copyChunks lbs)
  where
    copyChunks !L.Empty                         !_pf = return ()
    copyChunks !(L.Chunk (S.PS fpbuf o l) lbs') !pf  = do
        withForeignPtr fpbuf $ \pbuf ->
            copyBytes pf (pbuf `plusPtr` o) l
        copyChunks lbs' (pf `plusPtr` l)

-- | Run the builder to construct a strict bytestring containing the sequence
-- of bytes denoted by the builder. This is done by first serializing to a lazy bytestring and then packing its
-- chunks to a appropriately sized strict bytestring.
--
-- > toByteString = packChunks . toLazyByteString
--
-- Note that @'toByteString'@ is a 'Monoid' homomorphism.
--
-- > toByteString mempty          == mempty
-- > toByteString (x `mappend` y) == toByteString x `mappend` toByteString y
--
-- However, in the second equation, the left-hand-side is generally faster to
-- execute.
--
toByteString :: Builder -> S.ByteString
toByteString = packChunks . toLazyByteString


-- | @toByteStringIOWith bufSize io b@ runs the builder @b@ with a buffer of
-- at least the size @bufSize@ and executes the 'IO' action @io@ whenever the
-- buffer is full.
--
-- Compared to 'toLazyByteStringWith' this function requires less allocation,
-- as the output buffer is only allocated once at the start of the
-- serialization and whenever something bigger than the current buffer size has
-- to be copied into the buffer, which should happen very seldomly for the
-- default buffer size of 32kb. Hence, the pressure on the garbage collector is
-- reduced, which can be an advantage when building long sequences of bytes.
--
toByteStringIOWith :: Int                      -- ^ Buffer size (upper bounds
                                               -- the number of bytes forced
                                               -- per call to the 'IO' action).
                   -> (S.ByteString -> IO ())  -- ^ 'IO' action to execute per
                                               -- full buffer, which is
                                               -- referenced by a strict
                                               -- 'S.ByteString'.
                   -> Builder                  -- ^ 'Builder' to run.
                   -> IO ()                    -- ^ Resulting 'IO' action.
toByteStringIOWith bufSize io (Builder b) = 
    fillNewBuffer bufSize (b finalStep)
  where
    finalStep pf _ = return $ Done pf

    fillNewBuffer !size !step0 = do
        S.mallocByteString size >>= fillBuffer
      where
        fillBuffer fpbuf = fill step0
          where
            -- safe because the constructed ByteString references the foreign
            -- pointer AFTER its buffer was filled.
            pf = unsafeForeignPtrToPtr fpbuf
            fill !step = do
                next <- step pf (pf `plusPtr` size)
                case next of
                    Done pf' ->
                        unless (pf' == pf) (io $  S.PS fpbuf 0 (pf' `minusPtr` pf))

                    BufferFull newSize pf' nextStep  -> do
                        io $ S.PS fpbuf 0 (pf' `minusPtr` pf)
                        if bufSize < newSize
                          then fillNewBuffer newSize nextStep
                          else fill nextStep
                        
                    ModifyChunks  pf' bsk nextStep  -> do
                        unless (pf' == pf) (io $  S.PS fpbuf 0 (pf' `minusPtr` pf))
                        -- was: mapM_ io $ L.toChunks (bsk L.empty)
                        L.foldrChunks (\bs -> (io bs >>)) (return ()) (bsk L.empty)
                        fill nextStep

-- | Run the builder with a 'defaultBufferSize'd buffer and execute the given
-- 'IO' action whenever the buffer is full or gets flushed.
--
-- @ 'toByteStringIO' = 'toByteStringIOWith' 'defaultBufferSize'@
--
-- This is a 'Monoid' homomorphism in the following sense.
--
-- > toByteStringIO io mempty          == return ()
-- > toByteStringIO io (x `mappend` y) == toByteStringIO io x >> toByteStringIO io y
--
toByteStringIO :: (S.ByteString -> IO ()) -> Builder -> IO ()
toByteStringIO = toByteStringIOWith defaultBufferSize
{-# INLINE toByteStringIO #-}

-}
