import Test.Hspec
import Test.QuickCheck
import Control.Monad (zipWithM_)
import Data.Maybe (isNothing, fromJust)
import ChordParserTest as CPT
import TuningTest as TT

main :: IO ()
main = sequence_ [CPT.mainChordParserTest, TT.mainTuningTest]
  
