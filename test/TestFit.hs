import qualified Test.Fit.Internal.FitFile as FitFile
import qualified Test.Fit.Internal.FitParser as FitParser
import qualified Test.Fit.Internal.Numbers as Numbers

import Test.Hspec

main :: IO ()
main = hspec $ do
  Numbers.specs
  FitParser.specs
  FitFile.specs
