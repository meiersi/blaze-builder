{-# LANGUAGE CPP, BangPatterns #-}
-- |
-- Module      : Blaze.ByteString.Builder.Internal.WriteIO
-- Copyright   : (c) 2010 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- A general and efficient write type that allows for the easy construction of
-- builders for (smallish) bounded size writes to a buffer.
--
module Blaze.ByteString.Builder.Internal.Write (
  -- * Abstracting writes to a buffer
    Write
  , WriteIO
  , writeN
  , exactWrite
  , boundedWrite

  -- * Constructing builders from writes
  , fromWrite
  , fromWriteSingleton
  , fromWriteList

  -- * Writing 'Storable's
  , writeStorable
  , fromStorable
  , fromStorables

  ) where

import Foreign

import Data.Char
import Data.Monoid

import Control.Monad

import Blaze.ByteString.Builder.Internal.Types


------------------------------------------------------------------------------
-- The Write WriteIO Type
------------------------------------------------------------------------------

-- Sadly GHC is not smart enough: code where we branch and each branch should
-- execute a few IO actions and then return a value cannot be taught to GHC. At
-- least not such that it returns the value of the branches unpacked.
--
-- Hmm.. at least he behaves much better for the Monoid instance of Write
-- than the one for WriteIO. Serializing UTF-8 chars gets a slowdown of a
-- factor 2 when 2 chars are composed. Perhaps I should try out the writeList
-- instances also, as they may be more sensitive to to much work per Char.
--

-- | A write to a buffer.
newtype WriteIO = 
    WriteIO { runWriteIO :: Ptr Word8 -> IO (Ptr Word8) }

-- | A write of a bounded number of bytes.
data Write = Write {-# UNPACK #-} !Int WriteIO


instance Monoid WriteIO where
  mempty = WriteIO $ return
  {-# INLINE mempty #-}
  (WriteIO w1) `mappend` (WriteIO w2) = WriteIO $ w1 >=> w2
  {-# INLINE mappend #-}
  mconcat = foldr mappend mempty
  {-# INLINE mconcat #-}

instance Monoid Write where
  mempty = Write 0 mempty
  {-# INLINE mempty #-}
  (Write bound1 w1) `mappend` (Write bound2 w2) =
    Write (bound1 + bound2) (w1 `mappend` w2)
  {-# INLINE mappend #-}
  mconcat = foldr mappend mempty
  {-# INLINE mconcat #-}


-- | @writeN size io@ creates a write that denotes the writing of @size@ bytes
-- to a buffer using the IO action @io@. Note that @io@ MUST write EXACTLY @size@
-- bytes to the buffer!
writeN :: Int 
       -> (Ptr Word8 -> IO ()) -> WriteIO
writeN size io = WriteIO $ \op -> io op >> return (op `plusPtr` size)
{-# INLINE writeN #-}


-- | @exactWrite size io@ creates a bounded write that can later be converted to
-- a builder that writes exactly @size@ bytes. Note that @io@ MUST write
-- EXACTLY @size@ bytes to the buffer!
exactWrite :: Int 
           -> (Ptr Word8 -> IO ()) 
           -> Write
exactWrite size io = Write size (writeN size io)
{-# INLINE exactWrite #-}

-- | @boundedWrite size write@ creates a bounded write from a @write@ that does
-- not write more than @size@ bytes.
boundedWrite :: Int -> WriteIO -> Write
boundedWrite = Write
{-# INLINE boundedWrite #-}

fromWrite :: Write -> Builder
fromWrite (Write maxSize wio) =
    fromBuildStepCont step
  where
    step k (BufRange op ope)
      | op `plusPtr` maxSize <= ope = do
          op' <- runWriteIO wio op
          let !br' = BufRange op' ope
          k br'
      | otherwise = return $ bufferFull maxSize op (step k)
{-# INLINE fromWrite #-}

fromWriteSingleton :: (a -> Write) -> a -> Builder
fromWriteSingleton write = 
    mkBuilder
  where
    mkBuilder x = fromBuildStepCont step
      where
        step k (BufRange op ope)
          | op `plusPtr` maxSize <= ope = do
              op' <- runWriteIO wio op
              let !br' = BufRange op' ope
              k br'
          | otherwise = return $ bufferFull maxSize op (step k)
          where
            Write maxSize wio = write x
{-# INLINE fromWriteSingleton #-}

-- | Construct a 'Builder' writing a list of data one element at a time.
fromWriteList :: (a -> Write) -> [a] -> Builder
fromWriteList write = 
    makeBuilder
  where
    makeBuilder xs0 = fromBuildStepCont $ step xs0
      where
        step xs1 k !(BufRange op0 ope0) = go xs1 op0
          where
            go [] !op = do
               let !br' = BufRange op ope0
               k br'

            go xs@(x':xs') !op
              | op `plusPtr` maxSize <= ope0 = do
                  !op' <- runWriteIO wio op
                  go xs' op'
              | otherwise = return $ bufferFull maxSize op (step xs k)
              where
                Write maxSize wio = write x'
{-# INLINE fromWriteList #-}



{-
------------------------------------------------------------------------------
-- Testing the abstraction
------------------------------------------------------------------------------

-- TODO: Move!

-- Utf-8 encoding
-----------------

bwriteChar :: Char -> Write
bwriteChar c = Write 4 (encodeCharUtf8 f1 f2 f3 f4 c)
  where
    f1 x1          = writeN 1 $ \op -> do pokeByteOff op 0 x1

    f2 x1 x2       = writeN 2 $ \op -> do pokeByteOff op 0 x1
                                          pokeByteOff op 1 x2
                   
    f3 x1 x2 x3    = writeN 3 $ \op -> do pokeByteOff op 0 x1
                                          pokeByteOff op 1 x2
                                          pokeByteOff op 2 x3

    f4 x1 x2 x3 x4 = writeN 4 $ \op -> do pokeByteOff op 0 x1
                                          pokeByteOff op 1 x2
                                          pokeByteOff op 2 x3
                                          pokeByteOff op 3 x4
{-# INLINE bwriteChar #-}

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

-}

------------------------------------------------------------------------------
-- Writing storables
------------------------------------------------------------------------------


-- | Write a storable value.
{-# INLINE writeStorable #-}
writeStorable :: Storable a => a -> Write 
writeStorable x = exactWrite (sizeOf x) (\op -> poke (castPtr op) x)

-- | A builder that serializes a storable value. No alignment is done.
{-# INLINE fromStorable #-}
fromStorable :: Storable a => a -> Builder
fromStorable = fromWriteSingleton writeStorable

-- | A builder that serializes a list of storable values by writing them
-- consecutively. No alignment is done. Parsing information needs to be
-- provided externally.
fromStorables :: Storable a => [a] -> Builder
fromStorables = fromWriteList writeStorable
