-- | Core types and functions for the 'Builder' monoid
module Text.Blaze.Builder.Core.Internal
    ( 
    -- * The Builder type
      Builder(..)
    , BuildStep
    , BuildSignal(..)

    -- * Internal constants
    , defaultBufferSize
    , minBufferLength
    , maxCopyLength
    ) where


import Foreign
import Data.Monoid
import qualified Data.ByteString as S


------------------------------------------------------------------------------
-- The Builder type
------------------------------------------------------------------------------

-- | Main builder type. It simply contains a function to extract the actual
-- data.
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
minBufferLength :: Int
minBufferLength = 4 * 1024

-- | The maximal number of bytes where copying is cheaper than direct
-- insertion. This takes into account the fragmentation that may occur in the
-- output buffer due to the early flush.
maxCopyLength :: Int
maxCopyLength = 2 * minBufferLength

