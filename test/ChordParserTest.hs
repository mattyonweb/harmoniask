module ChordParserTest where

import Harmoniask.Parser as HP
import Text.ParserCombinators.ReadP
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad (zipWithM_)
import Data.Maybe (isNothing, fromJust)


mainChordParserTest :: IO ()
mainChordParserTest = hspec $
  describe "HP.parser" $ do
    it "doesn't crash on syntactically correct chords" $ chords_prop
  

chords_prop = forAll genChord $
  \s -> length (stringToPitches s) > 0

------------------------------

-- Utile per ottenere (Gen String) al posto di (Gen Char)
split :: [a] -> [[a]]
split = map (\x -> [x])


genNote = elements $ split "ABCDEFGabcdefg"

genAccident = oneof [ Just <$> (elements $ split "#b♭")
                    , return Nothing ]
                      
genTonality = oneof [ Just <$> (elements $ split "+-")
                    , return Nothing ]

genColorForm = frequency [ (1, Just <$> elements ["sus2", "sus4", "7", "maj7"])
                         , (3, return Nothing) ]

genColorNotes = do
  let colorNotes = ["4", "5", "9", "11", "13"]
  chosenNotes <- sublistOf colorNotes
  accidents   <- vectorOf (length chosenNotes) genAccident

  return $ Just $ -- potresti anche ometterlo ma non farlo
    (concat $ zipWith (\n a -> if isNothing a
                              then n
                              else (fromJust a) ++ n) chosenNotes accidents)


genChord = do
  note <- genNote
  acc  <- genAccident
  ton  <- genTonality
  form <- genColorForm
  notes <- genColorNotes
  
  let rest = mconcat [acc, ton, form, notes] -- :)))

  if isNothing rest
  then return note
  else return $ note ++ fromJust rest
  

  
