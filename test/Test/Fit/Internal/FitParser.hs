module Test.Fit.Internal.FitParser (
  specs,
) where

import Control.Lens (ix, (^..), (^?))
import Data.Bits (shiftL, (.&.))
import qualified Data.ByteString as B
import Data.Either (fromRight)
import Data.Word (Word16)
import Fit.Internal.Architecture (Arch (ArchBig, ArchLittle))
import Fit.Internal.FitFile (
  TimeOffset (TO),
  Timestamp (Timestamp, unTimestamp),
 )
import Fit.Internal.FitParser (
  archWord16,
  runFitParser,
  storeTimestamp,
  updateTimestamp,
  withArchitecture,
  word8,
 )
import Fit.Messages (readFileMessages)
import Fit.Messages.Lens (field, message, real)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.Attoparsec (shouldParse, (~>))
import Test.QuickCheck hiding ((.&.))

specs :: Spec
specs = describe "Fit.Parse.FitParser" $ do
  architectureSpec
  timestampSpec
  goldenTests

architectureSpec :: Spec
architectureSpec = describe "Architecture" $ do
  it "Parses a single byte" $
    property $
      \n -> B.pack [n] ~> runFitParser word8 `shouldParse` n

  it "Parses little-endian by default" $
    property $
      \n -> B.pack [n, 0] ~> runFitParser archWord16 `shouldParse` (fromIntegral n :: Word16)

  it "Respects withArchitecture when parsing" $
    property $
      let parser = runFitParser $ withArchitecture ArchBig archWord16
       in \n -> B.pack [n, 0] ~> parser `shouldParse` (fromIntegral n `shiftL` 8 :: Word16)

  it "Nests withArchitecture calls correctly" $ do
    let parser = withArchitecture ArchBig $ do
          little <- withArchitecture ArchLittle archWord16
          big <- archWord16
          return (little, big)
    B.pack [1, 0, 1, 0] ~> runFitParser parser `shouldParse` (1, 256)

timestampSpec :: Spec
timestampSpec = describe "Timestamps" $ do
  -- Compressed timestamp offsets aren't zero-based. See FitParser.updateTimestamp
  -- or the FIT protocol document for details.
  it "Finds the previously stored Timestamp" $ do
    let timestamp = Timestamp 100
        nullOffset = TO . fromIntegral $ unTimestamp timestamp .&. 0x1F
        parser = runFitParser $ storeTimestamp timestamp >> updateTimestamp nullOffset

    B.empty ~> parser `shouldParse` timestamp

  -- Data from Figure 4.2 in the FIT spec rev. 1.7
  it "Decompresses timestamp series correctly" $ do
    let base = Timestamp 0x3B
        offsets = map TO [0x1B, 0x1D, 0x02, 0x05, 0x01]
        expectedStamps = map Timestamp [0x3B, 0x3D, 0x42, 0x45, 0x61]
        parser = runFitParser $ do
          storeTimestamp base
          mapM updateTimestamp offsets

    B.empty ~> parser `shouldParse` expectedStamps

goldenTests :: Spec
goldenTests = describe "Golden tests" $ do
  activity <-
    runIO $
      fromRight (error "Failed to read test activity file.")
        <$> readFileMessages "test/data/Activity.fit"

  it "Can parse file with developer data" $
    let doughnuts = activity ^.. message 18 ^? ix 0 . field 0 . real
     in doughnuts `shouldBe` Just 3.00083327293396
