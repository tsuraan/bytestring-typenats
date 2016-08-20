{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, OverloadedStrings #-}
-- |
-- Module : Data.ByteString.TypeLits
-- Copyright: Jeremy Groven
-- License: BSD3
--
-- A tiny tagged 'ByteString' wrapper that carries around the size of the
-- wrapped ByteString. This allows for very simple
-- serialization/deserialization.
module Data.ByteString.TypeLits
( ByteString
, wrap
, length
, append
) where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Serialize.Put as SP
import qualified Data.Serialize.Get as SG
import qualified Data.Binary.Put as BP
import qualified Data.Binary.Get as BG

import Control.DeepSeq      ( NFData(..) )
import Crypto.Hash.Skein512 ( hash )
import Data.Binary          ( Binary(..) )
import Data.Coerce          ( coerce )
import Data.Monoid          ( (<>) )
import Data.Proxy           ( Proxy(..) )
import Data.Serialize       ( Serialize(..) )
import Data.String          ( IsString(..) )
import Test.QuickCheck      ( Arbitrary(..), Gen, choose, vectorOf )
import GHC.TypeLits

import Prelude hiding ( length )

-- | A sized-tagged wrapper around 'ByteString's. Useful when doing a lot of
-- serialization and deserialization of bytestrings that have fixed lengths.
newtype ByteString (sz :: Nat ) = ByteString
  { stripSize :: ByteString.ByteString }
  deriving ( Eq, Ord )

-- |Wrap a 'ByteString' with a size tag. Returns 'Just' if the given ByteString
-- has the desired length, 'Nothing' if it does not.
wrap :: KnownNat sz => ByteString.ByteString -> Either String (ByteString sz)
wrap bs =
  let dummy = undefined
      r     = wrap' dummy
  in fst' (r, dummy)
  where
  wrap' :: KnownNat sz => ByteString sz -> Either String (ByteString sz)
  wrap' dummy =
    let needLen = fromIntegral $ natVal $ proxy dummy
        gotLen  = ByteString.length bs
    in if gotLen == needLen
      then Right $ ByteString bs
      else Left $ "wrap needed " ++ show needLen ++ " bytes, got " ++ show gotLen

-- | Wrap a 'ByteString' with a size tag. Errors out if the given ByteString is
-- the wrong size, does the wrapping if the size is right.
wrap' :: KnownNat sz => ByteString.ByteString -> ByteString sz
wrap' bs = case wrap bs of
            Right s  -> s
            Left err -> error err

-- | Determine the length of a size tagged ByteString. I think this is probably
-- constant-folded at compile time, since it only uses types.
length :: KnownNat sz => ByteString sz -> Int
length = fromIntegral . natVal . proxy

-- | Append two tagged 'ByteString's.
append :: (KnownNat s1, KnownNat s2)
       => ByteString s1 -> ByteString s2 -> ByteString (s1+s2)
append (ByteString a) (ByteString b) = ByteString (a <> b)

proxy :: ByteString sz -> Proxy sz
proxy _ = Proxy

fst' :: (a, a) -> a
fst' = fst

fastRandBs :: Int -> Gen ByteString.ByteString
fastRandBs 0 = return ""
fastRandBs numBytes | numBytes <= 16 = slowRandBs numBytes
fastRandBs numBytes = do
  seed <- slowRandBs 16 -- 128 bits of random seed
  let stream = LazyByteString.fromChunks $ iterate hashes seed
      trunc  = LazyByteString.take (toEnum numBytes) stream
  return $ LazyByteString.toStrict trunc
  where
  hashes input = hash (32*1024) (ByteString.take 16 input)

-- | Use choose to generate some "random" Word8 values, and then pack them
-- together with ByteString.pack
slowRandBs :: Int -> Gen ByteString.ByteString
slowRandBs numBytes = ByteString.pack `fmap` vectorOf numBytes (choose (0, 255))

instance Show (ByteString sz) where
  showsPrec x b = showsPrec x (stripSize b)

instance KnownNat sz => IsString (ByteString sz) where
  fromString = wrap' . BSC.pack

instance KnownNat sz => Serialize (ByteString sz) where
  put (ByteString bs) = SP.putByteString bs

  get = get' undefined
    where
    get' :: KnownNat sz => ByteString sz -> SG.Get (ByteString sz)
    get' dummy = wrap' <$> SG.getByteString (length dummy)

instance KnownNat sz => Binary (ByteString sz) where
  put (ByteString bs) = BP.putByteString bs
  putList sbss =
    let bss = coerce sbss
        bs  = ByteString.concat bss
    in BP.putByteString bs

  get = get' undefined
    where
    get' :: KnownNat sz => ByteString sz -> BG.Get (ByteString sz)
    get' dummy = wrap' <$> BG.getByteString (length dummy)

instance NFData (ByteString sz) where
  rnf (ByteString bs) = rnf bs

instance KnownNat sz => Arbitrary (ByteString sz) where
  arbitrary = arb' undefined
    where
    arb' :: KnownNat sz => ByteString sz -> Gen (ByteString sz)
    arb' dummy = wrap' `fmap` fastRandBs (length dummy)
