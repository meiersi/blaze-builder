{-# LANGUAGE CPP, BangPatterns #-}
-- |
-- Module      : BoundedWrite
-- Copyright   : (c) 2010 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- A more general/efficient write type.
--
module BoundedWrite (main) where

import Foreign
import Data.Monoid
import Data.Char

import Foreign.UPtr

import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L

import Blaze.ByteString.Builder.Internal
import Blaze.ByteString.Builder.Write 
import Blaze.ByteString.Builder.Word 

import Criterion.Main

------------------------------------------------------------------------------
-- Benchmarks
------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ concat
    {-
    [ benchmark "mconcat . map (fromWriteSingleton writeChar)"
        bfrom3Chars
        from3Chars
        chars3
    ]
    -}
    [ benchmark "mconcat . map fromWord8"
        (mconcat . map bfromWord8)
        (mconcat . map fromWord8)
        word8s
    ]
  where
    benchmark name boundedF staticF x =
        [ bench (name ++ " <- bounded write") $
            whnf (L.length . toLazyByteString . boundedF) x
        , bench (name ++ " <- static write") $
            whnf (L.length . toLazyByteString . staticF) x
        ]

word8s :: [Word8]
word8s = take 100000 $ cycle [0..]
{-# NOINLINE word8s #-}

chars :: [Char]
chars = take 100000 $ ['\0'..]
{-# NOINLINE chars #-}

chars2 :: [(Char,Char)]
chars2 = zip chars chars
{-# NOINLINE chars2 #-}

chars3 :: [(Char, Char, Char)]
chars3 = zip3 chars (reverse chars) (reverse chars)
{-# NOINLINE chars3 #-}

bfromChars = (mconcat . map (fromBWriteSingleton bwriteChar))
{-# NOINLINE bfromChars #-}

fromChars = (mconcat . map (fromWriteSingleton writeChar))
{-# NOINLINE fromChars #-}

bfrom2Chars = (mconcat . map (fromBWriteSingleton (\(c1, c2) -> bwriteChar c1 `mappend` bwriteChar c2)))
{-# NOINLINE bfrom2Chars #-}

from2Chars = (mconcat . map (fromWriteSingleton (\(c1, c2) -> writeChar c1 `mappend` writeChar c2)))
{-# NOINLINE from2Chars #-}

bfrom3Chars = (mconcat . map (fromBWriteSingleton (\(c1, c2, c3) -> bwriteChar c1 `mappend` bwriteChar c2 `mappend` bwriteChar c3)))
{-# NOINLINE bfrom3Chars #-}

from3Chars = (mconcat . map (fromWriteSingleton (\(c1, c2, c3) -> writeChar c1 `mappend` writeChar c2 `mappend` writeChar c3)))
{-# NOINLINE from3Chars #-}

------------------------------------------------------------------------------
-- The Bounded Write Type
------------------------------------------------------------------------------

-- * GRRR* GHC is too 'clever'... code where we branch and each branch should
-- execute a few IO actions and then return a value cannot be taught to GHC. 
-- At least not such that it returns the value of the branches unpacked.
--
-- Hmm.. at least he behaves much better for the Monoid instance of BWrite
-- than the one for Write. Serializing UTF-8 chars gets a slowdown of a
-- factor 2 when 2 chars are composed. Perhaps I should try out the writeList
-- instances also, as they may be more sensitive to to much work per Char.
--
data BWrite = BWrite {-# UNPACK #-} !Int (UPtr -> UPtr)

newtype UWrite = UWrite { runUWrite :: UPtr -> UPtr }

instance Monoid UWrite where
  mempty = UWrite $ \x -> x
  {-# INLINE mempty #-}
  (UWrite uw1) `mappend` (UWrite uw2) = UWrite (\up -> uw2 (uw1 up))
  {-# INLINE mappend #-}

instance Monoid BWrite where
  mempty = BWrite 0 (\x -> x)
  {-# INLINE mempty #-}
  (BWrite b1 io1) `mappend` (BWrite b2 io2) =
    BWrite (b1 + b2) (\op -> io2 (io1 op))
  {-# INLINE mappend #-}

execWrite :: IO () -> UPtr -> UPtr
execWrite io op' = S.inlinePerformIO io `seq` op'
{-# INLINE execWrite #-}

execWriteSize :: (Ptr Word8 -> IO ()) -> Int -> UPtr -> UPtr
execWriteSize io size op = execWrite (io (uptrToPtr op)) (op `plusUPtr` size)
{-# INLINE execWriteSize #-}

staticBWrite :: Int -> (Ptr Word8 -> IO ()) -> BWrite
staticBWrite size io = BWrite size (execWriteSize io size)
{-# INLINE staticBWrite #-}

bwriteWord8 :: Word8 -> BWrite 
bwriteWord8 x = staticBWrite 1 (`poke` x)
{-# INLINE bwriteWord8 #-}

fromBWrite :: BWrite -> Builder
fromBWrite (BWrite size io) =
    Builder step
  where
    step k !pf !pe
      | pf `plusPtr` size <= pe = do
          let !pf' = io (ptrToUPtr pf)
          k (uptrToPtr pf') pe
      | otherwise = return $ BufferFull size pf (step k)
{-# INLINE fromBWrite #-}

fromBWriteSingleton :: (a -> BWrite) -> a -> Builder
fromBWriteSingleton write = 
    mkPut
  where
    mkPut x = Builder step
      where
        step k !pf !pe
          | pf `plusPtr` size <= pe = do
              let !pf' = io (ptrToUPtr pf)
              k (uptrToPtr pf') pe
          | otherwise               = return $ BufferFull size pf (step k)
          where
            BWrite size io = write x
{-# INLINE fromBWriteSingleton #-}

bfromWord8 :: Word8 -> Builder
bfromWord8 = fromBWriteSingleton bwriteWord8

-- Utf-8 encoding
-----------------

bwriteChar :: Char -> BWrite
bwriteChar c = BWrite 4 (encodeCharUtf8 f1 f2 f3 f4 c)
  where
    f1 x =  \uptr -> execWrite (do let !ptr = uptrToPtr uptr
                                   poke ptr x )
                               (uptr `plusUPtr` 1)

    f2 x1 x2 = \uptr -> execWrite (do let !ptr = uptrToPtr uptr
                                      poke ptr x1
                                      poke (ptr `plusPtr` 1) x2 )
                                  (uptr `plusUPtr` 2)

    f3 x1 x2 x3 = \uptr -> execWrite (do let !ptr = uptrToPtr uptr
                                         poke ptr x1
                                         poke (ptr `plusPtr` 1) x2
                                         poke (ptr `plusPtr` 2) x3 )
                                     (uptr `plusUPtr` 3)

    f4 x1 x2 x3 x4 = \uptr -> execWrite (do let !ptr = uptrToPtr uptr
                                            poke ptr x1
                                            poke (ptr `plusPtr` 1) x2
                                            poke (ptr `plusPtr` 2) x3
                                            poke (ptr `plusPtr` 3) x4 )
                                        (uptr `plusUPtr` 4)
{-# INLINE bwriteChar #-}

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

