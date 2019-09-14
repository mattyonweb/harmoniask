module Harmoniask where

import ZMidi.Core
import Data.List (sort, elemIndex, concatMap)
import Data.Maybe (fromJust)

-- |This datatype codifies interval distances.
data Distance = Unison | SecondMin | SecondMaj | ThirdMin | ThirdMaj | Fourth | Tritone | Fifth | SixthMin | SixthMaj | SeventhMin | SeventhMaj | Octave | NinthMin | NinthMaj | TenthMin | TenthMaj | Eleventh | OctaveTritone | Twelfth | ThirteenthMin | ThirteenthMaj | DoubleSeventhMin | DoubleSeventhMaj | DoubleOctave
  deriving (Show, Eq, Enum, Ord)


data Accident = Sharp | Flat deriving (Show, Eq)

  
-- |Codifies intervals and their possible movement up or down.
data Interval =
    Up Distance
  | Down Distance
  | Interval Distance -- Lo so. È brutto ma necessario per creare linee melodiche altalenanti
  deriving (Show, Eq)

instance Enum Interval where
  fromEnum (Up i) = fromEnum i
  fromEnum (Down i) = - (fromEnum i)
  fromEnum (Interval i) = fromEnum i

  toEnum n = Interval (toEnum n) -- Non bello.
  
instance Ord Interval where
  compare (Up x) (Up y) = compare x y
  compare (Down x) (Down y) = compare y x
  compare (Interval x) (Interval y) = compare x y
  
  compare (Up x) (Interval y) = compare x y
  compare (Interval x) (Up y) = compare x y
  
  compare (Up x) _ = GT
  compare (Down x) _ = LT
  -- Manca!

-- |Used only for convenience, not important
data Note = C | Cs | Db | D | Ds | Eb | E | F | Fs | Gb | G | Gs | Ab | A | As | Bb | B
  deriving (Show, Eq, Read)

type Pitch = Int -- Midi pitch (C4 = 60)


-- | Self-explanatory.
mapEq :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapEq is f = map (\x -> if is x then f x else x)

-- | Rotates a list and applies a function to the piece of list pasted at the end.
rotateWith :: Ord a => (a -> a) -> Int -> [a] -> [a]
rotateWith func n xs = sort $ newStart ++ (map func newEnd) 
  where newEnd   = take n xs
        newStart = drop n xs


        
-------------------------
-- Notes and intervals --
-------------------------
midiToNote x = names !! ((x - 24) `rem` 12)
  where names = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

noteToMidi :: Note -> Pitch
noteToMidi C = 60
noteToMidi Cs = 61
noteToMidi Db = 61
noteToMidi D = 62
noteToMidi Ds = 63
noteToMidi Eb = 63
noteToMidi E = 64
noteToMidi F = 65
noteToMidi Fs = 66
noteToMidi Gb = 66
noteToMidi G = 67
noteToMidi Gs = 68
noteToMidi Ab = 68
noteToMidi A = 69
noteToMidi As = 70
noteToMidi Bb = 70
noteToMidi B = 71


distance :: Pitch -> Pitch -> Int
distance n1 n2 = abs $ n1 - n2

-- | Jump from a MIDI pitch to another by an interval.
jump :: Pitch -> Interval -> Pitch
jump note interval = note + fromEnum interval

-- | Builds a melodic line; every note has an interval relative to the *beginning* note.
line :: Pitch -> [Interval] -> [Pitch]
line note intervals = map (jump note) intervals

-- | Alters an interval (eg. 11 -> 11#, 9 -> 9b...)
--   Mostly used for chords.
alter :: Interval -> Accident -> Interval
alter int Flat  = toEnum (fromEnum int - 1)
alter int Sharp = toEnum (fromEnum int + 1)


-------------------------
-- Chords fundamentals --
-------------------------
data Chord = Chord [Interval] deriving (Eq, Show)

-- | How many notes a chord has.
numNotes :: Chord -> Int
numNotes (Chord is) = length is

-- | Returns the n-th inversion of a chord.
inversion :: Int -> Chord -> Chord
inversion num (Chord is) = Chord $ rotate num is
  where rotate n xs = take (length xs) . drop n . cycle $ xs



-- Combinators for creating chords.

-- | Adds an interval on top.
addOnTop :: Interval -> Chord -> Chord
addOnTop i (Chord is) = Chord $ is ++ [i]

-- | Alters an interval in the chord.
alterChord :: Interval -> Accident -> Chord -> Chord
alterChord i accident (Chord is) = Chord $ mapEq (== i) (flip alter accident) is 

-- | Removes an interval from the chord.
remove :: Interval -> Chord -> Chord
remove interval (Chord is) = Chord $ filter (/= interval) is

-- | Sorts the chord. (useful?)
sorted :: Chord -> Chord
sorted (Chord is) = Chord $ Data.List.sort is

-- | Combines chords combinators.
(|>) :: (Chord -> Chord) -> (Chord -> Chord) -> Chord -> Chord
(|>) = flip (.)



-- Real Chords
majTriad = Chord [Interval Unison, Interval ThirdMaj, Interval Fifth]
minTriad = Chord [Interval Unison, Interval ThirdMin, Interval Fifth]

x_7    = addOnTop (Interval SeventhMin)
x_maj7 = addOnTop (Interval SeventhMaj)
x_9    = addOnTop (Interval NinthMaj)
x_11   = addOnTop (Interval Eleventh)
x_13   = addOnTop (Interval ThirteenthMaj)
sus4   = remove (Interval ThirdMaj) |> remove (Interval ThirdMin) |> addOnTop (Interval Fourth) |> sorted
soWhat = Chord $ map Interval [ThirdMaj, SixthMaj, NinthMaj, Twelfth, DoubleSeventhMaj]

-- | Realizes a chord, that is to say: given a Chord and a tonic, builds the Midi-pitch
--   rapresentation of the chord.
realizeChord :: Chord -> Pitch -> [Pitch]
realizeChord (Chord is) tonic = line tonic is



-------------------------
-- Sequences of chords --
-------------------------
data Sequence = Sequence [Interval] [Chord] deriving (Show, Eq)

-- Sequences combinators
removeLastSeq :: Sequence -> Sequence
removeLastSeq (Sequence is cs) = Sequence (init is) (init cs)

substitution :: (Interval, Chord) -> (Interval, Chord) -> Sequence -> Sequence
substitution old new (Sequence is cs) = Sequence (map fst subbed) (map snd subbed)
  where couples = zip is cs
        subbed  = mapEq (== old) (\_ -> new) couples
  
tritoneSub :: Sequence -> Sequence
tritoneSub = substitution
             (Interval Fifth, x_7 majTriad)
             (Interval SecondMin, x_7 majTriad)
                 
soWhatSub :: Sequence -> Sequence
soWhatSub = substitution
            (Interval Unison, x_maj7 majTriad)
            (Interval Unison, soWhat)
            
-- rootlessVoicing :: Sequence -> Sequence
-- rootlessVoicing (Sequence _ cs) = map (
(|>>|) :: (Sequence -> Sequence) -> (Sequence -> Sequence) -> Sequence -> Sequence
(|>>|) = (.)


-- Concretize a Sequence by translating it to numerical MIDI values
realizeSeq :: Pitch -> Sequence -> [[Pitch]]
realizeSeq tonica (Sequence intervals chords) =
    zipWith (flip realizeChord) bassline chords
  where bassline = line tonica intervals


-- Concretize a Sequence by translating it to numerical MIDI values
realizeSeqCompact :: Pitch -> Sequence -> [[Pitch]]
realizeSeqCompact tonica (Sequence intervals chords) = scanl1 shortestPathMidi realizedChords
  where bassline       = line tonica intervals
        realizedChords = zipWith (flip realizeChord) bassline chords
        
inversionMidi :: [Pitch] -> [[Pitch]]
inversionMidi ns = concat $ map -- use non-deterministic monad? >>= on list
                   (\rot -> [rotateWith id rot ns,
                             rotateWith (+12) rot ns,
                             map (subtract 12) $ rotateWith (+12) rot ns])
                   [0..length ns]

shortestPathMidi :: [Pitch] -> [Pitch] -> [Pitch]
shortestPathMidi c1 c2 = inversions' !! indexMin
  where inversions' = inversionMidi c2
        ratedInversions = map (zipWith distance c1) inversions'
        indexMin  = fromJust $ elemIndex (minimum ratedInversions) ratedInversions



-- -- Real sequences
v_i    = Sequence [Interval Fifth, Interval Unison]
                  [x_7 majTriad, x_maj7 majTriad]
ii_v_i = Sequence [Interval SecondMaj, Interval Fifth, Interval Unison]
                  [x_7 minTriad, x_7 majTriad, x_maj7 majTriad]
ii_v_tritone = (removeLastSeq |>>| tritoneSub) ii_v_i



data Harmony = Harmony [Interval] [Sequence] --tonalità - accordi

emptyHarmony :: Sequence -> Harmony
emptyHarmony seq = Harmony [Interval Unison] [seq]

circleFifths :: Harmony
circleFifths = Harmony ints seqs
  where ints = replicate 12 (Down SecondMaj)
        seqs = replicate 12 ii_v_i

coltrane :: Harmony
coltrane = Harmony ints seqs
  where ints = concat $ replicate 3 [Up ThirdMaj, Down SixthMin]
        seqs = replicate 3 v_i

realizeHarmony :: Pitch -> Harmony -> [[Pitch]]
realizeHarmony note harmony = realizeHarmonyWith note realizeSeq harmony 
  
realizeHarmonyWith :: Pitch -> (Pitch -> Sequence -> [[Pitch]]) -> Harmony -> [[Pitch]]
realizeHarmonyWith note seqRealizer (Harmony is seq) =
  concat $ map (\(tonica, s) -> seqRealizer tonica s) zipped
  where bassline = scanl (\acc i -> jump acc i) note is
        zipped   = zip bassline seq




-- ** MIDI EXPORTING

-- | Creates a MidiFile given a track
createMidiFile :: MidiTrack -> MidiFile
createMidiFile track = MidiFile
  (MidiHeader MF0 1 (TPB 80))
  [track]

makeTrack :: [[Pitch]] -> MidiTrack
makeTrack chords = MidiTrack $
  trackTemplate ++ concatMap midiChorder chords

-- | Translates Chords Pitches in Midi signals
midiChorder :: [Pitch] -> [MidiMessage]
midiChorder c = concat $ [notesOn, notesOff]
  where notesOn  = map (\pitch -> (0, VoiceEvent RS_OFF (NoteOn 0 (fromIntegral pitch) 100))) c
        notesOff = map (\pitch -> (16, VoiceEvent RS_OFF (NoteOff 0 (fromIntegral pitch) 64))) c

  
exampleTrack :: MidiTrack
exampleTrack = makeTrack $
  realizeHarmonyWith (noteToMidi C) realizeSeqCompact circleFifths
  
trackTemplate :: [MidiMessage]
trackTemplate = [
  (0,MetaEvent (TextEvent SEQUENCE_NAME "")),
  (0,MetaEvent (SetTempo 500000)),
  (0,MetaEvent (TimeSignature 4 2 24 8)),
  (0,VoiceEvent RS_OFF (ProgramChange 0 0)),
  (0,VoiceEvent RS_OFF (Controller 0 10 64)),
  (0,VoiceEvent RS_ON (Controller 0 93 0)),
  (0,VoiceEvent RS_ON (Controller 0 7 100)),
  (0,VoiceEvent RS_ON (Controller 0 91 0))]


writers :: IO ()
writers = writeMidi "diocane.mid" $ createMidiFile $ exampleTrack

writeNewMidiFile :: MidiFile -> IO ()
writeNewMidiFile = writeMidi "diocane.mid" 
