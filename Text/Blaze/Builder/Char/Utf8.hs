{-# OPTIONS_GHC -fno-warn-unused-imports #-} 
-- ignore warning from 'import Data.Text.Encoding'


-- | 'Write's and 'Builder's for serializing Unicode characters using the UTF-8
-- encoding. 
--
module Text.Blaze.Builder.Char.Utf8
    ( 
      -- * Writing UTF-8 encoded characters to a buffer
      writeChar

      -- * Creating Builders from UTF-8 encoded characters
    , fromChar
    , fromString
    , fromShow
    , fromText
    ) where

import Foreign
import Data.Char (ord)

import Data.Text (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T -- imported for documentation links

import Text.Blaze.Builder.Internal
import Text.Blaze.Builder.Write

-- | Write a UTF-8 encoded Unicode character to a buffer.
--
-- Note that the control flow of 'writeChar' is more complicated than the one
-- of 'writeWord8', as the size of the write depends on the 'Char' written.
-- Therefore,
--
-- > fromWrite $ writeChar a `mappend` writeChar b
--
-- must not always be faster than
--
-- > fromChar a `mappend` fromChar b
--
-- Use benchmarking to make informed decisions.
--
writeChar :: Char -> Write
writeChar = encodeCharUtf8 f1 f2 f3 f4
  where
    f1 x = Write 1 $ \ptr -> poke ptr x

    f2 x1 x2 = Write 2 $ \ptr -> do poke ptr x1
                                    poke (ptr `plusPtr` 1) x2

    f3 x1 x2 x3 = Write 3 $ \ptr -> do poke ptr x1
                                       poke (ptr `plusPtr` 1) x2
                                       poke (ptr `plusPtr` 2) x3

    f4 x1 x2 x3 x4 = Write 4 $ \ptr -> do poke ptr x1
                                          poke (ptr `plusPtr` 1) x2
                                          poke (ptr `plusPtr` 2) x3
                                          poke (ptr `plusPtr` 3) x4
{-# INLINE writeChar #-}

-- | Encode a Unicode character to another datatype, using UTF-8. This function
-- acts as an abstract way of encoding characters, as it is unaware of what
-- needs to happen with the resulting bytes: you have to specify functions to
-- deal with those.
--
encodeCharUtf8 :: (Word8 -> a)                             -- ^ 1-byte UTF-8
               -> (Word8 -> Word8 -> a)                    -- ^ 2-byte UTF-8
               -> (Word8 -> Word8 -> Word8 -> a)           -- ^ 3-byte UTF-8
               -> (Word8 -> Word8 -> Word8 -> Word8 -> a)  -- ^ 4-byte UTF-8
               -> Char                                     -- ^ Input 'Char'
               -> a                                        -- ^ Result
encodeCharUtf8 f1 f2 f3 f4 c = case ord c of
    x | x <= 0x7F -> f1 $ fromIntegral x
      | x <= 0x07FF ->
           let x1 = fromIntegral $ (x `shiftR` 6) + 0xC0
               x2 = fromIntegral $ (x .&. 0x3F)   + 0x80
           in f2 x1 x2
      | x <= 0xFFFF ->
           let x1 = fromIntegral $ (x `shiftR` 12) + 0xE0
               x2 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
               x3 = fromIntegral $ (x .&. 0x3F) + 0x80
           in f3 x1 x2 x3
      | otherwise ->
           let x1 = fromIntegral $ (x `shiftR` 18) + 0xF0
               x2 = fromIntegral $ ((x `shiftR` 12) .&. 0x3F) + 0x80
               x3 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
               x4 = fromIntegral $ (x .&. 0x3F) + 0x80
           in f4 x1 x2 x3 x4
{-# INLINE encodeCharUtf8 #-}

-- | /O(1)/. Serialize a Unicode character using the UTF-8 encoding.
--
fromChar :: Char -> Builder
fromChar = fromWriteSingleton writeChar

-- | /O(n)/. Serialize a Unicode 'String' using the UTF-8 encoding.
--
fromString :: String -> Builder
fromString = fromWrite1List writeChar
-- Performance note: ^^^
--
--   fromWrite2List made things slightly worse for the blaze-html benchmarks
--   despite being better when serializing only a list.  Probably, the cache is
--   already occupied enough with dealing with the data from Html rendering.
--


-- | /O(n)/. Serialize a value by 'Show'ing it and UTF-8 encoding the resulting
-- 'String'.
--
fromShow :: Show a => a -> Builder
fromShow = fromString . show

-- | /O(n)/. Serialize a Unicode 'Text' using the UTF-8 encoding.
--
-- Note that this function is currently faster than 'T.encodeUtf8' provided by
-- "Data.Text.Encoding". Moreover, 'fromText' is also lazy, while 'T.encodeUtf8'
-- is strict.
--
fromText :: Text -> Builder
fromText = fromString . T.unpack
{-# INLINE fromText #-}

