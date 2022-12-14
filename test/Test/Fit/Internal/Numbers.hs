module Test.Fit.Internal.Numbers (
  specs,
) where

import Data.Bits (shiftL)
import qualified Data.ByteString as B
import Data.Int (Int16)
import Data.Word (Word8)
import Fit.Internal.Numbers (nByteIntBe, nByteIntLe)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Attoparsec (shouldParse, (~>))
import Test.QuickCheck (property)

specs :: Spec
specs = describe "Fit.Parse.Numbers" $ do
  nByteIntLeSpec
  nByteIntBeSpec

nByteIntLeSpec :: Spec
nByteIntLeSpec = describe "Little endian" $ do
  it "Correctly parses a single byte" $ do
    property $ \n ->
      B.pack [n]
        ~> nByteIntLe 1
        `shouldParse` (fromIntegral n :: Word8)

  it "Correctly parses a multi-byte Int" $ do
    property $ \n m ->
      B.pack [n, m]
        ~> nByteIntLe 2
        `shouldParse` ((fromIntegral m `shiftL` 8) + fromIntegral n :: Int16)

nByteIntBeSpec :: Spec
nByteIntBeSpec = describe "Big endian" $ do
  it "Correctly parses a single byte" $ do
    property $ \n ->
      B.pack [n]
        ~> nByteIntBe 1
        `shouldParse` (fromIntegral n :: Word8)

  it "Correctly parses a multi-byte Int" $ do
    property $ \n m ->
      B.pack [n, m]
        ~> nByteIntBe 2
        `shouldParse` ((fromIntegral n `shiftL` 8) + fromIntegral m :: Int16)
