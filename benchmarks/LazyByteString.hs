{-# LANGUAGE BangPatterns #-}
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

import Data.Word
import Data.Monoid 
import Data.List 

import Criterion.Main

import Foreign 
import qualified Data.ByteString               as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Internal as L

import Blaze.ByteString.Builder.Internal
import Blaze.ByteString.Builder.Write
import Blaze.ByteString.Builder.Word
import Blaze.ByteString.Builder.ByteString

------------------------------------------------------------------------------
-- Benchmarks
------------------------------------------------------------------------------

main :: IO ()
main = do
    let (chunkInfos, benchmarks) = unzip 
          [ lazyVsBlaze
              ( "filter ((==0) . (`mod` 3))"
              , L.filter ((==0) . (`mod` 3))
              , filterBlaze ((==0) . (`mod` 3))
              , (\i -> L.pack $ take i $ cycle [0..])
              , n)
          {-
          , lazyVsBlaze
              ( "concatMap (replicate 10)"
              , L.concatMap (L.replicate 10)
              , toLazyByteString . concatMapBuilder (fromReplicateWord8 10)
              , (\i -> L.pack $ take i $ cycle [0..])
              , n `div` 10 )
          -}
          , lazyVsBlaze 
              ( "unfoldr countToZero"
              , L.unfoldr    countToZero
              , unfoldrBlaze countToZero
              , id
              , n )
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
        [ mkBench implLazy  lazy
        , mkBench implBlaze blaze
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
    makeBuilder f x0 = Builder $ step x0
      where
        step x1 !k = fill x1
          where
            fill x !pf0 !pe0 = go (f x) pf0
              where
                go !Nothing        !pf = k pf pe0
                go !(Just (y, x')) !pf
                  | pf `plusPtr` size <= pe0 = do
                      io pf
                      go (f x') (pf `plusPtr` size)
                  | otherwise = return $ BufferFull size pf $ \pfNew peNew -> do 
                      io pfNew
                      fill x' (pfNew `plusPtr` size) peNew
                  where
                    !(Write size io) = write y
{-# INLINE fromWriteUnfoldr #-}

-- Filtering
------------

test i = 
    ((L.filter ((==0) . (`mod` 3)) $ x) ,
     (filterBlaze ((==0) . (`mod` 3)) $ x))
  where
    x = L.pack $ take i $ cycle [0..]

filterBlaze :: (Word8 -> Bool) -> L.ByteString -> L.ByteString
filterBlaze f = toLazyByteString . fromFilterLazy f

{-
filterBlaze2 :: (Word8 -> Bool) -> L.ByteString -> L.ByteString
filterBlaze2 f = 
  toLazyByteString . 
  L.foldrChunks (\c b -> fromFilterByteString f `mappend` b) mempty 
-}

fromFilterByteString :: (Word8 -> Bool) -> S.ByteString -> Builder
fromFilterByteString prop (S.PS ifp ioff ilen) = 
    Builder $ step
  where
    maxSpill = 1024
    !ip0 = unsafeForeignPtrToPtr ifp
    step !k !op0 !ope = 
        goBS (ip0 `plusPtr` ioff) ipe op0
      where
        !ipe = ip0 `plusPtr` ilen

        goBS !ip0 !op1
          | ip0    >= ipe      = do touchForeignPtr ifp -- input buffer consumed
                                    k op1 ope
          | ubSize <= free     = goPartial ip0 ipe op1
          | free   <= maxSpill = return $ BufferFull (2*maxSpill) op1 (goBS ip0 ipe)
          | otherwise          = goPartial ip0 (ip0 `plusPtr` free) op1
          where
            ubSize = ipe `minusPtr` ip0
            free   = ope `minusPtr` op1
            goPartial !ip1 !ipeTmp !op = go ip1
              where
                go !ip 
                  | ip < ipeTmp = do
                      w <- peek ip
                      case prop w of
                        True  -> do poke op w
                                    go (ip `plusPtr` 1) (op `plusPtr` 1)
                        False -> do go (ip `plusPtr` 1) op
                  | otherwise =
                      goBS ip op
{-# INLINE fromFilterByteString #-}

fromFilterLazy :: (Word8 -> Bool) -> L.ByteString -> Builder
fromFilterLazy prop lbs0 = 
    Builder $ step
  where
    step !k = goLBS lbs0
      where
        goLBS L.Empty                        !pf !pe = k pf pe
        goLBS (L.Chunk (S.PS ifp ioff ilen) lbs) !pf !pe = do
            let !ip = unsafeForeignPtrToPtr ifp
            goBS (ip `plusPtr` ioff) (ip `plusPtr` ilen) pf pe
          where
            -- (ip0, ipe0) input buffer (current, end)
            -- (op0, ope0) output buffer (current, end)
            goBS ip0 ipe op0 ope = copyNext ip0 op0
              where
                copyNext ip1 op = go ip1
                  where
                    go ip 
                      | ip < ipe = do
                          w <- peek ip
                          case prop w of
                            True 
                              | op < ope -> do poke op w
                                               copyNext (ip `plusPtr` 1) (op `plusPtr` 1)
                              | otherwise -> return $ BufferFull 1 op $ \pfNew peNew -> do
                                  poke pfNew w
                                  goBS (ip `plusPtr` 1) ipe (pfNew `plusPtr` 1) peNew
                            False -> go (ip `plusPtr` 1) 
                      | otherwise = do
                          touchForeignPtr ifp -- input buffer done => OK to release it
                          goLBS lbs op ope
{-# INLINE fromFilterLazy #-}


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
    makeBuilder !n0 x = Builder $ step 
      where
        Write size io = write x
        step !k = fill n0
          where
            fill !n1 !pf0 !pe0 = go n1 pf0
              where
                go 0 !pf = k pf pe0
                go n !pf
                  | pf `plusPtr` size <= pe0 = do
                      io pf
                      go (n-1) (pf `plusPtr` size)
                  | otherwise = return $ BufferFull size pf $ \pfNew peNew -> do 
                      io pfNew
                      fill (n-1) (pfNew `plusPtr` size) peNew
{-# INLINE fromWriteReplicated #-}

fromReplicateWord8 :: Int -> Word8 -> Builder
fromReplicateWord8 !n0 x = 
    Builder $ step
  where
    step !k = fill n0
      where
        fill !n !pf !pe
          | n <= 0    = k pf pe
          | pf' <= pe = do
              _ <- S.memset pf x (fromIntegral n) -- FIXME: This conversion looses information for 64 bit systems.
              k pf' pe
          | otherwise  = do
              let !l = pe `minusPtr` pf
              _ <- S.memset pf x (fromIntegral l) -- FIXME: This conversion looses information for 64 bit systems.
              return $ BufferFull 1 pe $ fill (n - l)
          where
            pf' = pf `plusPtr` n
{-# INLINE fromReplicateWord8 #-}


{-# RULES "fromWriteReplicated/writeWord8"
      fromWriteReplicated writeWord8 = fromReplicateWord8
 #-}


concatMapBuilder :: (Word8 -> Builder) -> L.ByteString -> Builder
concatMapBuilder f = L.foldr (\w b -> f w `mappend` b) mempty

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

