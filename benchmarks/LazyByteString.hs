{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- |
-- Module      : LazyByteString
-- Copyright   : (c) 2010 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Benchmarking of alternative implementations of functions in
-- Data.ByteString.Lazy that construct lazy bytestrings and cannot be
-- implemented with slicing only.
module LazyByteString where -- (main)  where

import Data.Char
import Data.Word
import Data.Monoid 
import Data.List 

import Control.Monad
import Control.Arrow (second)
import Criterion.Main

import Foreign 
import qualified Data.ByteString               as S
import qualified Data.ByteString.Unsafe        as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Internal as L

import Data.ByteString.Base64

import Blaze.ByteString.Builder.Internal
import Blaze.ByteString.Builder.Word
import Blaze.ByteString.Builder.ByteString

------------------------------------------------------------------------------
-- Benchmarks
------------------------------------------------------------------------------

main :: IO ()
main = do
    let (chunkInfos, benchmarks) = unzip 
          [ lazyVsBlaze
              ( "partitionLazy"
              , (uncurry mappend) . L.partition ((0 <) . sin . fromIntegral)
              , (uncurry mappend) . partitionLazy ((0 <) . sin . fromIntegral)
              , (\i -> L.drop 13 $ L.pack $ take i $ cycle [0..])
              , n)
          {-
          [ lazyVsBlaze
              ( "base64mime"
              , L.fromChunks . return . joinWith "\r\n" 76 . encode
              , toLazyByteString . encodeBase64MIME
              , (\i -> S.drop 13 $ S.pack $ take i $ cycle [0..])
              , n)
          -}
          {-
          [ lazyVsBlaze
              ( "joinWith"
              , L.fromChunks . return . joinWith "\r\n" 76
              , toLazyByteString . intersperseBlocks 76 "\r\n"
              , (\i -> S.drop 13 $ S.pack $ take i $ cycle [0..])
              , n)
          -}
          {-
          [ lazyVsBlaze
              ( "base64"
              , L.fromChunks . return . encode
              , toLazyByteString . encodeBase64
              , (\i -> S.drop 13 $ S.pack $ take i $ cycle [0..])
              , n)
          -}
          {-
          , lazyVsBlaze
              ( "copy"
              , L.copy
              , copyBlaze
              , (\i -> L.drop 13 $ L.take (fromIntegral i) $ L.fromChunks $ repeat $ S.pack [0..])
              , n)
          , lazyVsBlaze
              ( "filter ((==0) . (`mod` 3))"
              , L.filter ((==0) . (`mod` 3))
              , filterBlaze ((==0) . (`mod` 3))
              , (\i -> L.drop 13 $ L.pack $ take i $ cycle [0..])
              , n)
          , lazyVsBlaze
              ( "map (+1)"
              , L.map (+1)
              , mapBlaze (+1)
              , (\i -> L.drop 13 $ L.pack $ take i $ cycle [0..])
              , n)
          , lazyVsBlaze
              ( "concatMap (replicate 10)"
              , L.concatMap (L.replicate 10)
              , toLazyByteString . concatMapBuilder (fromReplicateWord8 10)
              , (\i -> L.pack $ take i $ cycle [0..])
              , n `div` 10 )
          , lazyVsBlaze 
              ( "unfoldr countToZero"
              , L.unfoldr    countToZero
              , unfoldrBlaze countToZero
              , id
              , n )
          -}
          ]
    sequence_ (intersperse (putStrLn "") chunkInfos)
    putStrLn ""
    defaultMain benchmarks
  where
    n :: Int
    n = 100000

lazyVsBlaze :: (String, a -> L.ByteString, a -> L.ByteString, Int -> a, Int)
        -> (IO (), Benchmark)
lazyVsBlaze (cmpName, lazy, blaze, prep, n) =
    ( do putStrLn $ cmpName ++ ": " ++ checkResults
         showChunksize implLazy  lazy
         showChunksize implBlaze blaze
    , bgroup cmpName 
        [ mkBench implBlaze blaze
        , mkBench implLazy  lazy
        ]
    )
  where
    implLazy  = "bytestring"
    implBlaze = "blaze-builder"
    x = prep n

    nInfo = "for n = " ++ show n
    checkResults 
      | lazy x == blaze x = "implementations agree " ++ nInfo
      | otherwise         = unlines [ "ERROR: IMPLEMENTATIONS DISAGREE " ++ nInfo
                                    , implLazy ++ ": " ++ show (lazy x)
                                    , implBlaze ++ ": " ++ show (blaze x)
                                    ]

    showChunksize implName impl = do
      let bs = impl x
          cs = map S.length $ L.toChunks bs
      putStrLn $ "  " ++ implName ++ ": "
      putStrLn $ "    chunks sizes:    " ++ show cs
      putStrLn $ "    avg. chunk size: " ++ 
        show ((fromIntegral (sum cs) :: Double) / fromIntegral (length cs))

    mkBench implName impl = bench implName $ whnf (L.length . impl) x


------------------------------------------------------------------------------
-- Alternative implementations
------------------------------------------------------------------------------

-- Unfolding
------------

{-
-- | /O(n)/ The 'unfoldr' function is analogous to the List \'unfoldr\'.
-- 'unfoldr' builds a ByteString from a seed value.  The function takes
-- the element and returns 'Nothing' if it is done producing the
-- ByteString or returns 'Just' @(a,b)@, in which case, @a@ is a
-- prepending to the ByteString and @b@ is used as the next element in a
-- recursive call.
unfoldr :: (a -> Maybe (Word8, a)) -> a -> ByteString
unfoldr f s0 = unfoldChunk 32 s0
  where unfoldChunk n s =
          case S.unfoldrN n f s of
            (c, Nothing)
              | S.null c  -> Empty
              | otherwise -> Chunk c Empty
            (c, Just s')  -> Chunk c (unfoldChunk (n*2) s')
-}

countToZero :: Int -> Maybe (Word8, Int)
countToZero 0 = Nothing
countToZero i = Just (fromIntegral i, i - 1)

unfoldrBlaze :: (a -> Maybe (Word8, a)) -> a -> L.ByteString
unfoldrBlaze f x = toLazyByteString $ fromWriteUnfoldr writeWord8 f x

fromWriteUnfoldr :: (b -> Write) -> (a -> Maybe (b, a)) -> a -> Builder
fromWriteUnfoldr write = 
    makeBuilder
  where
    makeBuilder f x0 = fromBuildStepCont $ step x0
      where
        step x1 !k = fill x1
          where
            fill x !(BufRange pf0 pe0) = go (f x) pf0
              where
                go !Nothing        !pf = do
                    let !br' = BufRange pf pe0
                    k br'
                go !(Just (y, x')) !pf
                  | pf `plusPtr` bound <= pe0 = do
                      !pf' <- runWrite (write y) pf
                      go (f x') pf'
                  | otherwise = return $ bufferFull bound pf $ 
                      \(BufRange pfNew peNew) -> do 
                          !pfNew' <- runWrite (write y) pfNew
                          fill x' (BufRange pfNew' peNew)
                  where
                    bound = getBound $ write y
{-# INLINE fromWriteUnfoldr #-}

-- Filtering and mapping
------------------------

test :: Int -> (L.ByteString, L.ByteString)
test i = 
    ((L.filter ((==0) . (`mod` 3)) $ x) ,
     (filterBlaze ((==0) . (`mod` 3)) $ x))
  where
    x = L.pack $ take i $ cycle [0..]

filterBlaze :: (Word8 -> Bool) -> L.ByteString -> L.ByteString
filterBlaze f = toLazyByteString . filterLazyByteString f
{-# INLINE filterBlaze #-}

mapBlaze :: (Word8 -> Word8) -> L.ByteString -> L.ByteString
mapBlaze f = toLazyByteString . mapLazyByteString f
{-# INLINE mapBlaze #-}

filterByteString :: (Word8 -> Bool) -> S.ByteString -> Builder
filterByteString p = mapFilterMapByteString id p id
{-# INLINE filterByteString #-}

filterLazyByteString :: (Word8 -> Bool) -> L.ByteString -> Builder
filterLazyByteString p = mapFilterMapLazyByteString id p id
{-# INLINE filterLazyByteString #-}

mapByteString :: (Word8 -> Word8) -> S.ByteString -> Builder
mapByteString f = mapFilterMapByteString f (const True) id
{-# INLINE mapByteString #-}

mapLazyByteString :: (Word8 -> Word8) -> L.ByteString -> Builder
mapLazyByteString f = mapFilterMapLazyByteString f (const True) id
{-# INLINE mapLazyByteString #-}

mapFilterMapByteString :: (Word8 -> Word8) -> (Word8 -> Bool) -> (Word8 -> Word8) 
                       -> S.ByteString -> Builder
mapFilterMapByteString f p g = 
    \bs -> fromBuildStepCont $ step bs
  where
    step (S.PS ifp ioff isize) !k = 
        goBS (unsafeForeignPtrToPtr ifp `plusPtr` ioff)
      where
        !ipe = unsafeForeignPtrToPtr ifp `plusPtr` (ioff + isize)
        goBS !ip0 !br@(BufRange op0 ope)
          | ip0 >= ipe = do touchForeignPtr ifp -- input buffer consumed
                            k br
          | op0 < ope  = goPartial (ip0 `plusPtr` min outRemaining inpRemaining)
          | otherwise  = return $ bufferFull 1 op0 (goBS ip0) 
          where
            outRemaining = ope `minusPtr` op0
            inpRemaining = ipe `minusPtr` ip0
            goPartial !ipeTmp = go ip0 op0
              where
                go !ip !op
                  | ip < ipeTmp = do
                      w <- peek ip
                      let w' = g w
                      if p w'
                        then poke op (f w') >> go (ip `plusPtr` 1) (op `plusPtr` 1)
                        else                   go (ip `plusPtr` 1) op
                  | otherwise =
                      goBS ip (BufRange op ope)
{-# INLINE mapFilterMapByteString #-}

mapFilterMapLazyByteString :: (Word8 -> Word8) -> (Word8 -> Bool) -> (Word8 -> Word8) 
                           -> L.ByteString -> Builder
mapFilterMapLazyByteString f p g = 
    L.foldrChunks (\c b -> mapFilterMapByteString f p g c `mappend` b) mempty
{-# INLINE mapFilterMapLazyByteString #-}


-- Concatenation and replication
--------------------------------

{-
-- | Map a function over a 'ByteString' and concatenate the results
concatMap :: (Word8 -> ByteString) -> ByteString -> ByteString
concatMap _ Empty        = Empty
concatMap f (Chunk c0 cs0) = to c0 cs0
  where
    go :: ByteString -> P.ByteString -> ByteString -> ByteString
    go Empty        c' cs' = to c' cs'
    go (Chunk c cs) c' cs' = Chunk c (go cs c' cs')

    to :: P.ByteString -> ByteString -> ByteString
    to c cs | S.null c  = case cs of
        Empty          -> Empty
        (Chunk c' cs') -> to c' cs'
            | otherwise = go (f (S.unsafeHead c)) (S.unsafeTail c) cs
-}

fromWriteReplicated :: (a -> Write) -> Int -> a -> Builder
fromWriteReplicated write = 
    makeBuilder
  where
    makeBuilder !n0 x = fromBuildStepCont $ step 
      where
        bound = getBound $ write x
        step !k = fill n0
          where
            fill !n1 !(BufRange pf0 pe0) = go n1 pf0
              where
                go 0 !pf = do
                    let !br' = BufRange pf pe0
                    k br'
                go n !pf
                  | pf `plusPtr` bound <= pe0 = do
                      pf' <- runWrite (write x) pf
                      go (n-1) pf'
                  | otherwise = return $ bufferFull bound pf $ 
                      \(BufRange pfNew peNew) -> do 
                          pfNew' <- runWrite (write x) pfNew
                          fill (n-1) (BufRange pfNew' peNew)
{-# INLINE fromWriteReplicated #-}

-- FIXME: Output repeated bytestrings for large replications.
fromReplicateWord8 :: Int -> Word8 -> Builder
fromReplicateWord8 !n0 x = 
    fromBuildStepCont $ step
  where
    step !k = fill n0
      where
        fill !n !br@(BufRange pf pe)
          | n <= 0    = k br
          | pf' <= pe = do
              _ <- S.memset pf x (fromIntegral n) -- FIXME: This conversion looses information for 64 bit systems.
              let !br' = BufRange pf' pe
              k br'
          | otherwise  = do
              let !l = pe `minusPtr` pf
              _ <- S.memset pf x (fromIntegral l) -- FIXME: This conversion looses information for 64 bit systems.
              return $ bufferFull 1 pe $ fill (n - l)
          where
            pf' = pf `plusPtr` n
{-# INLINE fromReplicateWord8 #-}


{-# RULES "fromWriteReplicated/writeWord8"
      fromWriteReplicated writeWord8 = fromReplicateWord8
 #-}


concatMapBuilder :: (Word8 -> Builder) -> L.ByteString -> Builder
concatMapBuilder f = L.foldr (\w b -> f w `mappend` b) mempty
{-# INLINE concatMapBuilder #-}

concatMapBlaze :: (Word8 -> L.ByteString) -> L.ByteString -> L.ByteString
concatMapBlaze f = toLazyByteString . concatMapBuilder (fromLazyByteString . f)


-- Interspersing
----------------

--
-- not sure if it Builder version is needed, as chunks get only bigger. We
-- would need it however, if we used a Builder to ensure latency guarantees; i.e.,
-- if we use a builder to ensure a bound on the maximal size of chunks.
--

{-
-- | The 'intersperse' function takes a 'Word8' and a 'ByteString' and
-- \`intersperses\' that byte between the elements of the 'ByteString'.
-- It is analogous to the intersperse function on Lists.
intersperse :: Word8 -> ByteString -> ByteString
intersperse _ Empty        = Empty
intersperse w (Chunk c cs) = Chunk (S.intersperse w c)
                                   (foldrChunks (Chunk . intersperse') Empty cs)
  where intersperse' :: P.ByteString -> P.ByteString
        intersperse' (S.PS fp o l) =
          S.unsafeCreate (2*l) $ \p' -> withForeignPtr fp $ \p -> do
            poke p' w
            S.c_intersperse (p' `plusPtr` 1) (p `plusPtr` o) (fromIntegral l) w
-}
{-
intersperseBlaze :: Word8         -- ^ Byte to intersperse.
                 -> L.ByteString  -- ^ Lazy 'L.ByteString' to be "spread".
                 -> Builder       -- ^ Resulting 'Builder'.
intersperseBlaze w lbs0 = 
    Builder $ step lbs0
  where
    step lbs1 k = goChunk lbs1
      where
        goChunk L.Empty                         pf0 pe0 = k pf0 pe0
        goChunk (L.Chunk (S.PS fpi oi li) lbs') pf0 pe0 = do
            go
            touch
          where
            go 
              where
                !pf' = pf `plusPtr` 
                
            
        goChunk !L.Empty                !pf = k pf pe0
        goChunk !lbs@(L.Chunk bs' lbs') !pf
          | pf' <= pe0 = do
              withForeignPtr fpbuf $ \pbuf -> 
                  copyBytes pf (pbuf `plusPtr` offset) size
              go lbs' pf'

          | otherwise  = return $ BufferFull size pf (step lbs k)
          where
            !pf' = pf `plusPtr` 
            !(fpbuf, offset, size) = S.toForeignPtr bs'
{-# INLINE intersperseBlaze #-}

-}


-- Packing
----------

packBlaze :: [Word8] -> L.ByteString
packBlaze = toLazyByteString . fromWriteList writeWord8


-- Reverse
----------


-- Transpose
------------


-- scanl, scanl1, scanr, scanr1
-------------------------------


-- mapAccumL, mapAccumR
-----------------------


-- partition
------------

-- unzip
--------


-- copy
-------

copyBlaze :: L.ByteString -> L.ByteString
copyBlaze = toLazyByteString . copyLazyByteString


-- ?? packCString, packCStringLen
---------------------------------

-- joinWith
--------------------------------------------

intersperseBlocks :: Int -> S.ByteString -> S.ByteString -> Builder
intersperseBlocks blockSize sep (S.PS ifp ioff isize) = 
    fromPut $ do
        lastBS <- go (ip0 `plusPtr` ioff) 
        unless (S.null lastBS) (putBuilder $ fromByteString lastBS)
  where
    ip0 = unsafeForeignPtrToPtr ifp
    ipe = ip0 `plusPtr` (ioff + isize)
    go !ip 
      | ip `plusPtr` blockSize >= ipe =
          return $ S.PS ifp (ip `minusPtr` ip0) (ipe `minusPtr` ip)
      | otherwise = do
          putBuilder $ fromByteString (S.PS ifp (ip `minusPtr` ip0) blockSize)
                       `mappend` fromByteString sep
          go (ip `plusPtr` blockSize)

intersperseLazyBlocks :: Int -> Builder -> L.ByteString -> Builder
intersperseLazyBlocks blockSize sep bs =
    go (splitLazyAt blockSize bs)
  where
    go (pre, suf)
      | L.null suf = fromLazyByteString pre
      | otherwise  = fromLazyByteString pre `mappend` sep `mappend`
                     go (splitLazyAt blockSize suf)

encodeBase64MIME :: S.ByteString -> Builder
encodeBase64MIME =
  intersperseLazyBlocks 76 (fromByteString "\r\n") . toLazyByteString . encodeBase64


-- test blockwise mapping on base64 encoding
--------------------------------------------

-- | Encode a bytestring using Base64 encoding according to the specification
-- in RFC 4648, <http://www.apps.ietf.org/rfc/rfc4648.html>.
--
-- Note that you need to insert additional linebreaks every 76 bytes using the
-- function @joinWith "\r\n" 76@ in order to achieve the MIME Base64
-- Content-Transfer-Encoding <specified in http://tools.ietf.org/html/rfc2045>.
--
-- TODO implement encoding of lazy bytestrings, implement joinWith
-- functionality, and convencience function for MIME base-64 encoding.
encodeBase64 :: S.ByteString -> Builder
encodeBase64 = encodeLazyBase64 . L.fromChunks . return

encodeLazyBase64 :: L.ByteString -> Builder
encodeLazyBase64 = 
    mkBuilder
  where
    mkBuilder bs = fromPut $ do
        remainder <- putWriteLazyBlocks 3 writeBase64 bs 
        putBuilder $ complete remainder

    {-# INLINE writeBase64 #-}
    writeBase64 ip = 
        exactWrite 4 $ \op -> do
            b0 <- peekByte 0
            b1 <- peekByte 1
            b2 <- peekByte 2
            let w = (b0 `shiftL` 16) .|. (b1 `shiftL` 8) .|. b2
            poke (castPtr $ op            ) =<< enc (w `shiftR` 12)
            poke (castPtr $ op `plusPtr` 2) =<< enc (w .&.   0xfff)
      where
        peekByte :: Int -> IO Word32
        peekByte off = fmap fromIntegral (peekByteOff ip off :: IO Word8)
        
        enc = peekElemOff (unsafeForeignPtrToPtr encodeTable) . fromIntegral

    {-# INLINE complete #-}
    complete bs
      | S.null bs = mempty
      | otherwise = fromWrite $
          exactWrite 4 $ \op -> do
              let poke6Base64 off sh = pokeByteOff op off
                      (alphabet `S.unsafeIndex` fromIntegral (w `shiftR` sh .&. 63))
                  pad off = pokeByteOff op off (fromIntegral $ ord '=' :: Word8)
              poke6Base64 0 18
              poke6Base64 1 12
              if S.length bs == 1 then pad 2 
                                  else poke6Base64 2 8
              pad 3
      where
        getByte :: Int -> Int -> Word32
        getByte i sh = fromIntegral (bs `S.unsafeIndex` i) `shiftL` sh
        w = getByte 0 16 .|. (if S.length bs == 1 then 0 else getByte 1 8)
            
    -- Lookup table trick from Data.ByteString.Base64 by Bryan O'Sullivan
    {-# NOINLINE alphabet #-}
    alphabet :: S.ByteString
    alphabet = S.pack $ [65..90] ++ [97..122] ++ [48..57] ++ [43,47]

    -- FIXME: Check that the implementation of the lookup table aslo works on
    -- big-endian systems.
    {-# NOINLINE encodeTable #-} 
    encodeTable :: ForeignPtr Word16
    encodeTable = unsafePerformIO $ do
        fp <- mallocForeignPtrArray 4096
        let ix = fromIntegral . S.index alphabet
        withForeignPtr fp $ \p ->
          sequence_ [ pokeElemOff p (j*64+k) ((ix k `shiftL` 8) .|. ix j)
                    | j <- [0..63], k <- [0..63] ]
        return fp


-- | Process a bytestring block-wise using a 'Write' action to produce the
-- output per block.
--
-- TODO: Compare speed with 'mapFilterMapByteString'.
{-# INLINE putWriteBlocks #-}
putWriteBlocks :: Int                  -- ^ Block size.
               -> (Ptr Word8 -> Write) -- ^ 'Write' given a pointer to the
                                       --   beginning of the block.
               -> S.ByteString         -- ^ 'S.ByteString' to consume blockwise.
               -> Put S.ByteString     -- ^ 'Put' returning the remaining
                                       --   bytes, which are guaranteed to be
                                       --   fewer than the block size.
putWriteBlocks blockSize write =
    \bs -> putBuildStepCont $ step bs
  where
    step (S.PS ifp ioff isize) !k = 
        goBS (unsafeForeignPtrToPtr ifp `plusPtr` ioff)
      where
        !ipe = unsafeForeignPtrToPtr ifp `plusPtr` (ioff + isize)
        goBS !ip0 !br@(BufRange op0 ope)
          | ip0 `plusPtr` blockSize > ipe = do 
              touchForeignPtr ifp -- input buffer consumed
              let !bs' = S.PS ifp (ip0 `minusPtr` unsafeForeignPtrToPtr ifp) 
                                  (ipe `minusPtr` ip0)
              k bs' br

          | op0 `plusPtr` writeBound < ope  = 
              goPartial (ip0 `plusPtr` (blockSize * min outRemaining inpRemaining))

          | otherwise  = return $ bufferFull writeBound op0 (goBS ip0) 
          where
            writeBound   = getBound' "putWriteBlocks" write 
            outRemaining = (ope `minusPtr` op0) `div` writeBound
            inpRemaining = (ipe `minusPtr` ip0) `div` blockSize

            goPartial !ipeTmp = go ip0 op0
              where
                go !ip !op
                  | ip < ipeTmp = do
                      op' <- runWrite (write ip) op
                      go (ip `plusPtr` blockSize) op'
                  | otherwise =
                      goBS ip (BufRange op ope)


{-# INLINE putWriteLazyBlocks #-}
putWriteLazyBlocks :: Int                  -- ^ Block size.
                   -> (Ptr Word8 -> Write) -- ^ 'Write' given a pointer to the
                                           --   beginning of the block.
                   -> L.ByteString         -- ^ 'L.ByteString' to consume blockwise.
                   -> Put S.ByteString     -- ^ 'Put' returning the remaining
                                           --   bytes, which are guaranteed to be
                                           --   fewer than the block size.
putWriteLazyBlocks blockSize write =
    go
  where
    go L.Empty          = return S.empty
    go (L.Chunk bs lbs) = do
      bsRem <- putWriteBlocks blockSize write bs
      case S.length bsRem of
        lRem 
          | lRem <= 0 -> go lbs
          | otherwise -> do
              let (lbsPre, lbsSuf) = 
                      L.splitAt (fromIntegral $ blockSize - lRem) lbs
              case S.concat $ bsRem : L.toChunks lbsPre of
                block@(S.PS bfp boff bsize)
                  | bsize < blockSize -> return block
                  | otherwise         -> do
                      putBuilder $ fromWrite $ 
                        write (unsafeForeignPtrToPtr bfp `plusPtr` boff)
                      putLiftIO $ touchForeignPtr bfp
                      go lbsSuf 
                          

------------------------------------------------------------------------------
-- Testing code
------------------------------------------------------------------------------


chunks3 :: [Word8] -> [Word32]
chunks3 (b0 : b1 : b2 : bs) = 
    ((fromIntegral b0 `shiftL` 16) .|. 
     (fromIntegral b1 `shiftL`  8) .|. 
     (fromIntegral b2            )
    ) : chunks3 bs
chunks3 _                   = []

cmpWriteToLib :: [Word8] -> (L.ByteString, L.ByteString)
cmpWriteToLib bs = 
    -- ( toLazyByteString $ fromWriteList write24bitsBase64 $ chunks3 bs
    ( toLazyByteString $ encodeBase64 $ S.pack bs
    , (`L.Chunk` L.empty) $ encode $ S.pack bs )

test3 :: Bool
test3 = uncurry (==) $ cmpWriteToLib $ [0..]

test2 :: L.ByteString 
test2 = toLazyByteString $ encodeBase64 $ S.pack [0..]

{- OLD code
 
{-# INLINE poke8 #-}
poke8 :: Word8 -> Ptr Word8 -> IO ()
poke8 = flip poke

-- | @writeBase64 w@ writes the lower @24@ bits as four times 6 bit in
-- little-endian order encoded using the standard alphabeth of Base 64 encoding
-- as defined in <http://www.apps.ietf.org/rfc/rfc4648.html>.
--
{-# INLINE write6bitsBase64 #-}
write6bitsBase64 :: Word32 -> Write
write6bitsBase64 = exactWrite 1  . poke6bitsBase64

{-# INLINE poke6bitsBase64 #-}
poke6bitsBase64 :: Word32 -> Ptr Word8 -> IO ()
poke6bitsBase64 w = poke8 (alphabet `S.unsafeIndex` fromIntegral (w .&. 63))
    {-
    | i <  26   = withOffsets  0 'A'
    | i <  52   = withOffsets 26 'a'
    | i <  62   = withOffsets 52 '0'
    | i == 62   = poke8 $ fromIntegral $ ord '+'
    | otherwise = poke8 $ fromIntegral $ ord '/'
  where
    i :: Int
    i = fromIntegral (w .&. 63)

    {-# INLINE withOffsets #-}
    withOffsets neg pos = poke8 $ fromIntegral (i + ord pos - neg)
    -}

{-# INLINE writePaddedBitsBase64 #-}
writePaddedBitsBase64 :: Bool             -- ^ Only 8 bits have to be output.
                      -> Word32           -- ^ Input whose lower 8 or 16 bits need to be output.
                      -> Write
writePaddedBitsBase64 only8 w =
    write6bitsBase64 (w `shiftr_w32` 18)                         `mappend`
    write6bitsBase64 (w `shiftr_w32` 12)                         `mappend`
    writeIf (const only8) (const $ C8.writeChar '=')
                          (write6bitsBase64 . (`shiftr_w32`  6)) 
                          w                                      `mappend`
    C8.writeChar '='

{-# INLINE write24bitsBase64 #-}
write24bitsBase64 :: Word32 -> Write
write24bitsBase64 w = write6bitsBase64 (w `shiftr_w32` 18) `mappend`
                      write6bitsBase64 (w `shiftr_w32` 12) `mappend`
                      write6bitsBase64 (w `shiftr_w32`  6) `mappend`
                      write6bitsBase64 (w                )

-- ASSUMES bits 25 - 31 are zero.
{-# INLINE write24bitsBase64' #-}
write24bitsBase64' :: Word32 -> Write
write24bitsBase64' w = 
    exactWrite 4 $ \p -> do
      poke (castPtr p              ) =<< enc (w `shiftR` 12)
      poke (castPtr $ p `plusPtr` 2) =<< enc (w .&.   0xfff)
  where
    {-# INLINE enc #-}
    enc = peekElemOff (unsafeForeignPtrToPtr encodeTable) . fromIntegral

-}

-------------------------------------------------------------------------------
-- A faster split for lazy bytestrings
-------------------------------------------------------------------------------

-- | /O(n\/c)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
splitLazyAt :: Int -> L.ByteString -> (L.ByteString, L.ByteString)
splitLazyAt n cs0
  | n <= 0    = (L.Empty, cs0)
  | otherwise = split cs0
  where
    split L.Empty        = (L.Empty, L.Empty)
    split (L.Chunk c cs)
      | n < len  = case S.splitAt    n         c  of
          (pre, suf) -> (L.Chunk pre L.Empty, L.Chunk suf cs)
      | otherwise = case splitLazyAt (n - len) cs of
          (pre, suf) -> (L.Chunk c   pre    , suf           )
      where
        len = S.length c


-------------------------------------------------------------------------------
-- A faster partition for strict and lazy bytestrings
-------------------------------------------------------------------------------

{-# INLINE partitionStrict #-}
partitionStrict :: (Word8 -> Bool) -> S.ByteString -> (S.ByteString, S.ByteString)
partitionStrict f (S.PS ifp ioff ilen) = 
    second S.reverse $ S.inlinePerformIO $ do
        ofp <- S.mallocByteString ilen
        withForeignPtr ifp $ wrapper ofp
  where
    wrapper !ofp !ip0 = 
        go (ip0 `plusPtr` ioff) op0 (op0 `plusPtr` ilen)
      where
        op0 = unsafeForeignPtrToPtr ofp

        go !ip !opl !oph
          | oph == opl = return (S.PS ofp 0 olen, S.PS ofp olen (ilen - olen))
          | otherwise  = do
              x <- peek ip
              if f x 
                then do poke opl x 
                        go (ip `plusPtr` 1) (opl `plusPtr` 1) oph
                else do let oph' = oph `plusPtr` (-1)
                        poke oph' x
                        go (ip `plusPtr` 1) opl               oph'

          where
            olen = opl `minusPtr` op0

{-# INLINE partitionLazy #-}
partitionLazy :: (Word8 -> Bool) -> L.ByteString -> (L.ByteString, L.ByteString)
partitionLazy f = 
    L.foldrChunks partitionOne (L.empty, L.empty)
  where
    partitionOne bs (ls, rs) = 
        (L.Chunk l ls, L.Chunk r rs)
      where
        (l, r) = partitionStrict f bs
