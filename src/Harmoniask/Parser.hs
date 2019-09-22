module Harmoniask.Parser where

import Harmoniask
import Text.ParserCombinators.ReadP
import Data.Maybe (fromJust)
import Data.Char (toUpper)

readAccident :: Char -> Maybe Accident
readAccident 'b' = Just Flat
readAccident '#' = Just Sharp
readAccident '♭' = Just Flat
readAccident _   = Nothing

data Tonality = Major | Minor
  deriving (Eq, Show)

data ColorNote = ColorNote Interval (Maybe Accident)
  deriving (Eq, Show)
readColorNoteDiatonic :: String -> Interval
readColorNoteDiatonic "4" = Fourth
readColorNoteDiatonic "5" = Fifth
readColorNoteDiatonic "9" = NinthMaj
readColorNoteDiatonic "11" = Eleventh
readColorNoteDiatonic "13" = ThirteenthMaj
readColorNoteDiatonic x    = error "This should not happen"

data ColorForm = Sus2 | Sus4 | Simple7 | Maj7 | NoColorForm
  deriving (Eq, Show)
readColorForm :: String -> ColorForm
readColorForm "sus2" = Sus2
readColorForm "sus4" = Sus4
readColorForm "7" = Simple7
readColorForm "maj7" = Maj7


data ParsedChord = ParsedChord
  { tonik      :: (Note, Maybe Accident)
  , tonality   :: Tonality
  , colorForm  :: ColorForm
  , colorNotes :: [ColorNote] 
  } deriving (Eq, Show)


parsedToPitches :: ParsedChord -> [Pitch]
parsedToPitches (ParsedChord (tonic, mayAcc) tonality form colorNotes) =
  realizeChord chordIntervals tonicPitch
  
  where tonicPitch = noteToMidi tonic +
          (case mayAcc of
              Nothing -> 0
              Just Sharp -> 1
              Just Flat  -> -1)
        baseChord = case tonality of
          Major -> majTriad
          Minor -> minTriad
        specialForm = case form of
          Sus2 -> sus4
          Sus4 -> sus4
          Maj7 -> x_maj7
          Simple7 -> x_7
          NoColorForm -> id
        colNs = map (\(ColorNote i acc) ->
                       if acc == Nothing
                       then i
                       else alterInterval i (fromJust acc)) colorNotes
        addNoteComb = foldr (|>) id (map addOnTop colNs)
        chordIntervals = (addNoteComb |> specialForm) baseChord
          
        
------------------------------------------

isDiatonicNote :: Char -> Bool
isDiatonicNote char =
  any (char ==) "ABCDEFGabcdefg"

isAccident :: Char -> Bool
isAccident char =
  any (char ==) "b#♭"


-- | __C#__-maj7b9b13
--   ^^ 
tonicParser :: ReadP (Note, Maybe Accident)
tonicParser = do
  n <- fmap (\x -> (read [toUpper x]) :: Note) $ satisfy isDiatonicNote
  a <- option ' ' (satisfy isAccident)

  return (n, readAccident a)

-- | C#__-__maj7b9b13
--     ^ 
tonalityParser :: ReadP Tonality
tonalityParser = do
  ton <- option '+' (satisfy (\c -> any (c ==) "+-"))
  return $ if ton == '+' then Major else Minor

-- | C#-maj7__b9b13__
--          ^^^^^
colorNoteParser :: ReadP ColorNote
colorNoteParser = do
  a <- option ' ' (satisfy isAccident)
  n <- fmap readColorNoteDiatonic $ choice $ map string ["4", "5", "9", "11", "13"]
  return $ ColorNote n (readAccident a)

-- | C#-__maj7__b9b13
colorFormParser :: ReadP ColorForm
colorFormParser =
  fmap readColorForm $ choice $ map string $ ["7", "maj7", "sus2", "sus4"]

  

chord :: ReadP ParsedChord
chord = do
  tonik <- tonicParser
  ton   <- option Major tonalityParser
  form  <- option NoColorForm colorFormParser
  cols  <- many colorNoteParser
  return $ ParsedChord tonik ton form cols
  
stringToPitches :: String -> Either String [Pitch]
stringToPitches s = fmap parsedToPitches $ result $ readP_to_S chord s
  where result [] = Left $ "ERROR: the chord " ++ s ++ " could not be parsed."
        result xs = case last xs of
            (x, []) -> Right $ fst $ last xs
            (x, ys) -> Left  $ "ERROR: could not parse this section " ++ ys ++ " of the chord " ++ s

stringToPitchesUnsafe :: String -> [Pitch]
stringToPitchesUnsafe = parsedToPitches . fst . last . readP_to_S chord
