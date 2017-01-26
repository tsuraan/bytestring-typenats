{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, OverloadedStrings,
             LambdaCase, DeriveDataTypeable #-}
-- |
-- Module : Data.ByteString.TypeNats
-- Copyright: Jeremy Groven
-- License: BSD3
--
-- A tiny tagged 'ByteString' wrapper that carries around the size of the
-- wrapped ByteString. This allows for very simple
-- serialization/deserialization.
module Data.ByteString.TypeNats
( ByteString(stripSize)
, wrap
, wrap'
, random
, length
, append
, sizeHelper
, sizeHelper'
#ifdef UseArbitrary
, fastRandBs
, slowRandBs
#endif
) where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BSC
-- import qualified Data.ByteString.Lazy as LazyByteString

import Control.DeepSeq      ( NFData(..) )
-- import Crypto.Hash.Skein512 ( hash )
import Data.Functor.Identity ( Identity(..) )
import Data.Monoid          ( (<>) )
import Data.Proxy           ( Proxy(..) )
import Data.String          ( IsString(..) )
import Data.Typeable        ( Typeable )
import System.Entropy       ( getEntropy )
import GHC.TypeLits

import Prelude hiding ( length )

#ifdef UseCereal
import qualified Data.Serialize.Put as SP
import qualified Data.Serialize.Get as SG
import Data.Serialize       ( Serialize(..) )
#endif

#ifdef UseBinary
import qualified Data.Binary.Put as BP
import qualified Data.Binary.Get as BG
import Data.Binary          ( Binary(..) )
import Data.Coerce          ( coerce )
#endif

#ifdef UseArbitrary
import Crypto.Hash.BLAKE2.BLAKE2bp ( hash )
import Test.QuickCheck      ( Arbitrary(..), Gen, choose, vectorOf )
#endif

-- | A sized-tagged wrapper around 'ByteString's. Useful when doing a lot of
-- serialization and deserialization of bytestrings that have fixed lengths.
newtype ByteString (sz :: Nat ) = ByteString
  { stripSize :: ByteString.ByteString }
  deriving ( Eq, Ord, Typeable )

-- |Wrap a 'ByteString' with a size tag. Returns 'Just' if the given ByteString
-- has the desired length, 'Nothing' if it does not.
wrap :: KnownNat sz => ByteString.ByteString -> Either String (ByteString sz)
wrap bs = runIdentity $ sizeHelper (const $ return bs)

-- | Wrap a 'ByteString' with a size tag. Errors out if the given ByteString is
-- the wrong size, does the wrapping if the size is right.
wrap' :: KnownNat sz => ByteString.ByteString -> ByteString sz
wrap' bs = case wrap bs of
            Right wrapped -> wrapped
            Left err      -> error err

-- | Generate a random bytestring using the system's entropy source
random :: KnownNat sz => IO (ByteString sz)
random = sizeHelper' getEntropy

-- | Determine the length of a size tagged ByteString. I think this is probably
-- constant-folded at compile time, since it only uses types.
length :: KnownNat sz => ByteString sz -> Int
length = fromIntegral . natVal . proxy

-- | Append two tagged 'ByteString's.
append :: (KnownNat s1, KnownNat s2)
       => ByteString s1 -> ByteString s2 -> ByteString (s1+s2)
append (ByteString a) (ByteString b) =
  -- I really want to use wrap', but I can't make the type machinery work.
  -- Something about having to be 10% smarter than the tools we use... :(
  ByteString (a <> b)

proxy :: ByteString sz -> Proxy sz
proxy _ = Proxy

fst' :: (a, a) -> a
fst' = fst

-- | Generate a ByteString of the desired length. The given callback will be
-- called with the number of bytes that are indicated by the output's type, and
-- is expected to return a (standard) ByteString of that length.
sizeHelper :: (Monad m, KnownNat sz)
           => (Int -> m ByteString.ByteString)
           -> m (Either String (ByteString sz))
sizeHelper fn = do
  let dummy = undefined
  r <- helper' fn dummy
  return $ fst' (r, dummy)
  where
  helper' :: (Monad m, KnownNat sz)
          => (Int -> m ByteString.ByteString)
          -> ByteString sz
          -> m (Either String (ByteString sz))
  helper' fn' dummy = do
    let needLen = fromIntegral $ natVal $ proxy dummy
    bs <- fn' needLen
    let gotLen  = ByteString.length bs
    if gotLen == needLen
      then return $ Right $ ByteString bs
      else return $ Left $ "Expected " ++ show needLen ++ 
                      " byte result from callback, but got " ++ show gotLen

-- | Same as sizeHelper, but this does the monad's fail action instead of
-- returning Left/Right values
sizeHelper' :: (Monad m, KnownNat sz)
            => (Int -> m ByteString.ByteString)
            -> m (ByteString sz)
sizeHelper' fn =
  sizeHelper fn >>= \case
    Right bs -> return bs
    Left err -> fail err

instance Show (ByteString sz) where
  showsPrec x b = showsPrec x (stripSize b)

instance KnownNat sz => IsString (ByteString sz) where
  fromString = wrap' . BSC.pack

instance NFData (ByteString sz) where
  rnf (ByteString bs) = rnf bs

#ifdef UseCereal
instance KnownNat sz => Serialize (ByteString sz) where
  put (ByteString bs) = SP.putByteString bs

  get = sizeHelper' SG.getByteString
#endif

#ifdef UseBinary
instance KnownNat sz => Binary (ByteString sz) where
  put (ByteString bs) = BP.putByteString bs
  putList sbss =
    let bss = coerce sbss
        bs  = ByteString.concat bss
    in BP.putByteString bs

  get = sizeHelper' BG.getByteString
#endif

#ifdef UseArbitrary
-- | Quickly generate large amounts of pseudo-random data using a few bytes
-- from the quicktest generator, and then a bunch more bytes from Blake2.
fastRandBs :: Int -> Gen ByteString.ByteString
fastRandBs 0 = return ""
fastRandBs numBytes | numBytes <= 16 = slowRandBs numBytes
fastRandBs numBytes = hash numBytes "" <$> slowRandBs 16

-- | Use choose to generate some "random" Word8 values, and then pack them
-- together with ByteString.pack
slowRandBs :: Int -> Gen ByteString.ByteString
slowRandBs numBytes = ByteString.pack `fmap` vectorOf numBytes (choose (0, 255))

instance KnownNat sz => Arbitrary (ByteString sz) where
  arbitrary = sizeHelper' fastRandBs
#endif

