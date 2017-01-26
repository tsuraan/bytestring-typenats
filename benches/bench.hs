{-# LANGUAGE DataKinds #-}
#ifdef UseArbitrary
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Criterion.Main

import Data.ByteString.TypeNats
#endif

main :: IO ()
#ifdef UseArbitrary
main = defaultMain
  [ bench "arbitrary"    $ nfIO $ action (arbitrary :: Gen (ByteString 10))
  , bench "arbitrary1M"  $ nfIO $ action (arbitrary :: Gen (ByteString 1000000))
  , bench "arbitrary10M" $ nfIO $ action (arbitrary :: Gen (ByteString 10000000))
  , bgroup "fastRandBs"
    [ bench "10B" $ nfIO $ action (fastRandBs 10)
    , bench "1KB" $ nfIO $ action (fastRandBs 1024)
    , bench "1MB" $ nfIO $ action (fastRandBs (1024*1024))
    ]
  , bgroup "slowRandBs"
    [ bench "10B" $ nfIO $ action (slowRandBs 10)
    , bench "1KB" $ nfIO $ action (slowRandBs 1024)
    , bench "10KB" $ nfIO $ action (slowRandBs 10240)
    ]
  ]

  where
  action :: Gen a -> IO ()
  action act = do
    samples <- sample' act
    examine samples
    where
    examine :: [a] -> IO ()
    examine [] = return ()
    examine (hd:tl) = hd `seq` examine tl
#else
main = putStrLn "Arbitrary disabled; nothing to benchmark"
#endif

