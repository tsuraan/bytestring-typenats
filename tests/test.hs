import qualified Data.ByteString as BS
import Data.ByteString ( ByteString )
import Test.QuickCheck.Monadic
import Test.QuickCheck

import Data.ByteString.TypeNats ( fastRandBs, slowRandBs )

main :: IO ()
main = do
  quickCheck $ lengths fastRandBs
  quickCheck $ lengths (\i -> slowRandBs $ min i 10240)
  x <- generate $ fastRandBs 100000000
  putStrLn $ "100000000 " ++ (show $ BS.length x)
  where
  lengths :: (Int -> Gen ByteString) -> Int -> Property
  lengths genFn len =
    let len' = abs len
        gen  = genFn len'
    in monadicIO $ do
        samples <- run $ sample' gen
        assert $ all (==len') $ map BS.length samples

