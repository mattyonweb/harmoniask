module TuningTest where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad (zipWithM_)
import Data.Bits as DB
import Harmoniask.Tunings as HT


mainTuningTest :: IO ()
mainTuningTest = hspec $ do
  describe "HT (Harmoniask Tuning)" $ do
    zipWithM_ temperamentTest [eqHz, pytHz, werck1Hz, strangeHz]
                              ["Equable", "Pythagoric", "Werckmeister", "Strange"]

  describe "HT.midiFineTune, HT.freqsToCents, Ht.transform" $ do
    it "<distance in cents> -> <mst format> -> <distance in cents> = ID" $
      prop_cents

------------------------------------------------------

-- Test some properties of frequencies-pitch map
temperamentTest :: Temperament -> String -> SpecWith ()
temperamentTest temp name = describe name $ do
  it "has all the keys of a piano, from midi 21 to 104 (A0 to G#7)" $ do
    map fst pitchFreqs `shouldBe` [21..104]

  it "has all Word8 new base pitch values under 127" $ do
    mstMap `shouldSatisfy` (null . filter (\note -> (note !! 1) > 127))

  it "has all Word8 MSB cents values under 127" $ do
    mstMap `shouldSatisfy` (null . filter (\note -> (note !! 2) > 127))

  it "has all Word8 LSB cents values under 127" $ do
    mstMap `shouldSatisfy` (null . filter (\note -> (note !! 3) > 127))
    
  where pitchFreqs = HT.pianoRange temp
        mstMap     = HT.makeMap pitchFreqs


-- Test that, given two frequencies f1 and f2, going from their distance
-- in cents to their distance expressed in MST format to (again) their
-- cents distance gives the starting distance (modulo some floating
-- point approximations).
between :: Ord a => a -> a -> a -> Bool
between x a b = x > a && x < b

prop_cents :: Property
prop_cents = 
  forAll (suchThat arbitrary (\x -> x > 27.5)) $ \f1 ->
  forAll (suchThat arbitrary (\x -> (x > 27.5) &&
                               between (freqsToCents f1 x) 0 100)) $ \f2 ->
  (prop_cents_internal f1 f2)                                                                                          


prop_cents_internal :: Double -> Double -> Property
prop_cents_internal f1 f2 =  property $  
  (fromIntegral bitwise * 0.0061) `within` centsDist 
  where centsDist  = freqsToCents f1 f2        
        [msb, lsb] = map (\x -> (fromIntegral x) :: Int) $ midiFineTune centsDist

        bitwise :: Int
        bitwise = fromIntegral $ ((DB.shift msb 7) DB..|. (lsb DB..&. 127))

        within :: Real a => a -> a -> Bool
        within a b = abs (a - b) <= 1
