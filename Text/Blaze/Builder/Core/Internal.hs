-- | Implementation of the 'Builder' monoid.
module Text.Blaze.Builder.Core.Internal
    ( 
    -- * The Builder type
      Builder(..)
    , BuildStep
    , BuildSignal(..)

    -- * Default constants
    , defaultBufferSize
    , defaultMinimalChunkSize
    , defaultMaximalCopySize
    ) where


import Foreign
import Data.Monoid
import qualified Data.ByteString as S


------------------------------------------------------------------------------
-- The Builder type
------------------------------------------------------------------------------

-- | Intuitively, a builder denotes the construction of a lazy bytestring. 
--
-- Builders can be created from primitive buffer manipulations using the
-- 'Write' abstraction provided by in "Text.Blaze.Builder.Core". However for
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
-- prefetching in later processing steps (e.g. compression) or reduces the
-- sytem call overhead when writing the resulting lazy bytestring to a file or
-- sending it over the network.
--
-- For precisely understanding the performance of a specific 'Builder',
-- benchmarking is unavoidable. Moreover, it also helps to understand the
-- implementation of builders and the predefined combinators. This should be
-- amenable to the average Haskell programmer by reading the source code of
-- this library. The guiding implementation principle was to reduce the
-- abstraction cost per byte output as much as possible. Therefore, try to keep
-- the control flow during the evaluation of a builder as simple as possible.
-- We also try to take the pressure off the memory bus by moving variables as
-- far out of loops as possible. This leads to some duplication of code, but
-- results in sometimes dramatic increases in performance. For example, see the
-- @'fromWord8s'@ function in "Text.Blaze.Builder.Word".
--
newtype Builder = Builder (BuildStep -> BuildStep)

-- | A buildsignal is a signal returned from a write to the builder, it tells us
-- what should happen next.
--
data BuildSignal
  -- | Signal the completion of the write process.
  = Done {-# UNPACK #-} !(Ptr Word8)  -- ^ Pointer to the next free byte
  -- | Signal that the buffer is full and a new one needs to be allocated.
  -- It contains the minimal size required for the next buffer, a pointer to the
  -- next free byte, and a continuation.
  | BufferFull
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !(Ptr Word8)
      {-# UNPACK #-} !BuildStep
  -- | Signal that a ByteString needs to be wrapped or inserted as is. The
  -- concrete behaviour is up to the driver of the builder. Arguments:
  -- The next free byte in the buffer, the ByteString modification,
  -- and the next build step.
  | ModifyByteStrings
      {-# UNPACK #-} !(Ptr Word8) 
      {-# UNPACK #-} !([S.ByteString] -> [S.ByteString]) 
      {-# UNPACK #-} !BuildStep

-- | Type for a single build step. Every build step checks that
--
-- > free + bytes-written <= last
--
type BuildStep =  Ptr Word8       -- ^ Ptr to the next free byte in the buffer
               -> Ptr Word8       -- ^ Ptr to the first byte AFTER the buffer
               -> IO BuildSignal  -- ^ Signal the next step to be taken

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

-- | Copied from Data.ByteString.Lazy.
--
defaultBufferSize :: Int
defaultBufferSize = 32 * k - overhead
    where k = 1024
          overhead = 2 * sizeOf (undefined :: Int)

-- | The minimal length a buffer should have for it to be worth to be processed
-- further. For 4kb it is the case that a memcopy takes about as long as a
-- system call in Linux.
defaultMinimalChunkSize :: Int
defaultMinimalChunkSize = 4 * 1024

-- | The maximal number of bytes where copying is cheaper than direct
-- insertion. This takes into account the fragmentation that may occur in the
-- output buffer due to the early flush.
defaultMaximalCopySize :: Int
defaultMaximalCopySize = 2 * defaultMinimalChunkSize

