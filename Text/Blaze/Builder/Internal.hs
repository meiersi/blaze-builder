{-# LANGUAGE CPP, BangPatterns #-}
-- |
-- Module      : Text.Blaze.Builder.Internal
-- Copyright   : (c) 2010 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Implementation of the 'Builder' monoid.
--
-- A standard library user must never import this module directly. Instead, he
-- should import "Text.Blaze.Builder", which re-exports the 'Builder' type and
-- its associated public functions defined in this module.
--
-- Developers of other libraries may import this module to gain access to the
-- internal representation of builders. For example, in some cases, creating a
-- 'Builder' with a custom low-level 'BuildStep' may improve performance
-- considerably compared to the creating it using the public 'Builder'
-- combinators (e.g., @'fromWrite1List'@ in "Text.Blaze.Builder.Write").
-- Another example, is the use of 'ModifyChunks' to efficiently wire the
-- 'Builder' type with another library that generates lazy bytestrings.
--
-- In any case, whenever you import this module you /must/ reference the full
-- version of the 'blaze-builder' package in your cabal file, as the
-- implementation and the guarantees given in this file may change in any
-- version! The release notes will tell, if this was the case.
--
module Text.Blaze.Builder.Internal
    ( 
    -- * The @Builder@ type
      Builder(..)
    , BuildStep
    , BuildSignal(..)

    -- * Flushing the buffer
    , flush

    -- * Executing builders
    , toLazyByteStringWith
    , toLazyByteString
    , toByteString
    , toByteStringIOWith
    , toByteStringIO

    -- * Default sizes
    , defaultBufferSize
    , defaultMinimalBufferSize
    , defaultMaximalCopySize
    ) where


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


------------------------------------------------------------------------------
-- The Builder type
------------------------------------------------------------------------------

-- | Intuitively, a builder denotes the construction of a lazy bytestring. 
--
-- Builders can be created from primitive buffer manipulations using the
-- @'Write'@ abstraction provided by in "Text.Blaze.Builder.Write". However for
-- many Haskell values, there exist predefined functions doing that already. 
-- For example, UTF-8 encoding 'Char' and 'String' values is provided by the
-- functions in "Text.Blaze.Builder.Char.Utf8". Concatenating builders is done
-- using their 'Monoid' instance.
--
-- Semantically, builders are nothing special. They just denote a sequence of
-- bytes. However, their representation is chosen such that this sequence of
-- bytes can be efficiently (in terms of CPU cycles) computed in an
-- incremental, chunk-wise fashion such that the average chunk-size is large.
-- Note that the large average chunk size allows to make good use of cache
-- prefetching in later processing steps (e.g. compression) or to reduce the
-- sytem call overhead when writing the resulting lazy bytestring to a file or
-- sending it over the network.
--
-- For precisely understanding the performance of a specific 'Builder',
-- benchmarking is unavoidable. Moreover, it also helps to understand the
-- implementation of builders and the predefined combinators. This should be
-- amenable to the average Haskell programmer by reading the source code of
-- "Text.Blaze.Builder.Internal" and the other modules of this library. 
--
-- The guiding implementation principle was to reduce the abstraction cost per
-- output byte. We use continuation passing to achieve a constant time append.
-- The output buffer is filled by the individual builders as long as possible.
-- They call each other directly when they are done and control is returned to
-- the driver (e.g., 'toLazyByteString') only when the buffer is full, a
-- bytestring needs to be inserted directly, or no more bytes can be written.
-- We also try to take the pressure off the cache by moving variables as far
-- out of loops as possible. This leads to some duplication of code, but
-- results in sometimes dramatic increases in performance. For example, see the
-- @'fromWord8s'@ function in "Text.Blaze.Builder.Word".
--
newtype Builder = Builder (BuildStep -> BuildStep)

-- | A 'BuildSignal' signals to the driver of the 'Builder' execution the next
-- step to be taken.
--
data BuildSignal =
  -- | @Done pf@ signals that the 'BuildStep' is finished and data has been
  -- written up to the next free byte @pf@.
    Done {-# UNPACK #-} !(Ptr Word8) 
  -- | @BufferFull newSize pf nextStep@ signals that the buffer is full and
  -- data has been written up to the next free byte @pf@. Moreover, the next
  -- build step to be executed @nextStep@ requires a buffer of at least size
  -- @newSize@ to execute successfully.
  --
  -- A driver /must/ guarantee that the buffer used to call @nextStep@ is at
  -- least of size @newSize@.
  | BufferFull
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !(Ptr Word8)
                     !BuildStep
  -- | @ModifyChunks pf fbs nextStep@ signals that the data written up to the
  -- next free byte @pf@ must be output and the remaining lazy bytestring that
  -- is produced by executing @nextStep@ must be modified using the function
  -- @fbs@.
  --
  -- This signal is used to insert bytestrings directly into the output stream.
  -- It can also be used to efficiently hand over control to another library
  -- for generating streams of strict bytestrings.
  | ModifyChunks
      {-# UNPACK #-} !(Ptr Word8) 
                     !(L.ByteString -> L.ByteString) 
                     !BuildStep

-- | A 'BuildStep' fills a buffer from the given start pointer as long as
-- possible and returns control to the caller using a 'BuildSignal', once it is
-- required.
--
type BuildStep =  Ptr Word8       -- ^ Pointer to the next free byte in the
                                  -- buffer. A 'BuildStep' must start writing
                                  -- its data from this address.
               -> Ptr Word8       -- ^ Pointer to the first byte /after/ the
                                  -- buffer.  A 'BuildStep' must never write
                                  -- data at or after this address.
               -> IO BuildSignal  -- ^ Signal to the driver about the next step
                                  -- to be taken.

instance Monoid Builder where
    mempty = Builder id
    {-# INLINE mempty #-}
    mappend (Builder f) (Builder g) = Builder $ f . g
    {-# INLINE mappend #-}
    mconcat = foldr mappend mempty
    {-# INLINE mconcat #-}

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
flush :: Builder
flush = Builder $ \k pf _ -> return $ ModifyChunks pf id k

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
-- A call 'toLazyByteStringWith bufSize minBufSize firstBufSize' will generate
-- a lazy bytestring according to the following strategy. First, we allocate
-- a buffer of size 'firstBufSize' and start filling it. If it overflows, we
-- allocate a buffer of size 'minBufSize' and *copy* the first buffer to it in
-- order to avoid generating a too small chunk. Finally, every next buffer will
-- be of size 'bufSize'. This, slow startup strategy is required to achieve
-- good speed for short (<200 bytes) resulting bytestrings, as for them the
-- allocation cost is of a large buffer cannot be compensated. Moreover, this
-- strategy also allows us to avoid spilling too much memory.
--
-- Note that setting 'firstBufSize >= minBufSize' implies that the first buffer
-- is no longer copied but allocated and filled directly. Hence, setting
-- 'firstBufSize = bufSize' means that all chunks will use an underlying buffer
-- of size 'bufSize'. This is recommended, if you know that you always output
-- more than 'minBufSize' bytes.
toLazyByteStringWith 
    :: Int           -- ^ Buffer size (upper-bounds the resulting chunk size).
    -> Int           -- ^ Minimal free buffer space for continuing filling
                     -- the same buffer after a 'flush' or a direct bytestring
                     -- insertion. This corresponds to the minimal desired
                     -- chunk size.
    -> Int           -- Size of the first buffer to be used and copied for
                     -- larger resulting sequences
    -> Builder       -- ^ Builder to run.
    -> L.ByteString  -- ^ Lazy bytestring to output after the builder is
                     -- finished.
    -> L.ByteString  -- ^ Resulting lazy bytestring
toLazyByteStringWith bufSize minBufSize firstBufSize (Builder b) k = 
    inlinePerformIO $ fillFirstBuffer (b finalStep)
  where
    finalStep pf _ = return $ Done pf
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
                  Done pf' 
                    | pf' == pf -> return k
                    | otherwise -> return $ L.Chunk (mkbs pf') k

                  BufferFull newSize pf' nextStep  -> do
                      let !l  = pf' `minusPtr` pf
                      fillNewBuffer (max (l + newSize) minBufSize) $
                          \pfNew peNew -> do
                              copyBytes pfNew pf l
                              nextStep (pfNew `plusPtr` l) peNew
                      
                  ModifyChunks  pf' bsk nextStep  ->
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
                    Done pf' 
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
-- @ 'toLazyByteString' = 'toLazyByteStringWith' 'defaultBufferSize' 'defaultMinimalBufferSize' L.empty@
--
-- Note that @'toLazyByteString'@ is a 'Monoid' homomorphism.
--
-- > toLazyByteString mempty          == mempty
-- > toLazyByteString (x `mappend` y) == toLazyByteString x `mappend` toLazyByteString y
--
-- However, in the second equation, the left-hand-side is generally faster to
-- execute.
--
toLazyByteString :: Builder -> L.ByteString
toLazyByteString b = toLazyByteStringWith 
    defaultBufferSize defaultMinimalBufferSize defaultFirstBufferSize b L.empty
{-# INLINE toLazyByteString #-}

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

