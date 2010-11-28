{-# LANGUAGE BangPatterns #-}
-- | Demonstrate the problem with IO not allowing for unlifted types.
--
--   TODO: Not yet finished.
module StrictIO where



loop :: Int -> Int -> IO ()
loop !i !c
    | i == 1    = print c
    | otherwise = do
        !i' <- subcases
        print i'
        loop i' (c+1)
  where
    subcases
      | i `mod` 2 == 0 = do
          print "even"
          return $ i `div` 2
      | otherwise      = do
          print "odd"
          return $ i + 1
    {-# INLINE subcases #-}


  

