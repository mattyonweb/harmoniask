{-# LANGUAGE NumericUnderscores #-}

module Harmoniask where

import ZMidi.Core
import Data.List (sort, elemIndex, concatMap)
import Data.Maybe (fromJust, isNothing)
import GHC.Word
import Harmoniask.Tunings as HT

{- |
A typeclass for things that can be jumped from.

It is useful because of the distinction between a "static" interval (`Interval`) and
a "dynamic" interval (`MelodicInterval`, that can represent an interval going up or down).

@jump@ jumps from a MIDI pitch by a distance of an interval.
-}
class MusicalInterval a where
  jump :: Pitch -> a -> Pitch

  
{- |
This datatype codifies interval distances.

It can be thought as a "static" interval, that is one that doesn't encode movement information.
It is used for `Chord`s and `Sequence`s tonics (see later).
-}
data Interval = Unison | SecondMin | SecondMaj | ThirdMin | ThirdMaj | Fourth | Tritone | Fifth | SixthMin | SixthMaj | SeventhMin | SeventhMaj | NinthMin | NinthMaj | TenthMin | TenthMaj | Eleventh | OctaveTritone | Twelfth | ThirteenthMin | ThirteenthMaj | DoubleSeventhMin | DoubleSeventhMaj | DoubleOctave
  deriving (Show, Eq, Enum, Ord)

{- |
By default, a jump of an `Interval` is a jump up.
-}
instance MusicalInterval Interval where
  jump note interval = note + fromEnum interval

  
{- |
This datatype codifies melodic intervals.

It can be thought as a "dynamic" interval, that is one that encodes movement informations
(either upward `Up` or downward `Down`).

It is used for constructing melodies "out of context" (aka. melodic lines not bounded to
a tonal center such as a tonic).
-}
data MelodicInterval =
    Up Interval   -- ^ An interval jumping up.
  | Down Interval -- ^ An interval jumping down.
  deriving (Show, Eq)

instance Enum MelodicInterval where
  fromEnum (Up i) = fromEnum i
  fromEnum (Down i) = - (fromEnum i)

  toEnum n
    | n >= 0    = Up $ toEnum n
    | otherwise = Down $ toEnum (-n)
  
instance Ord MelodicInterval where
  compare (Up x) (Up y)     = compare x y
  compare (Down x) (Down y) = compare y x

  compare (Up _) _   = GT
  compare (Down _) _ = LT

instance MusicalInterval MelodicInterval where
  jump note (Up interval)   = note + fromEnum interval
  jump note (Down interval) = note - fromEnum interval
  

{- |
Used only for convenience, not really important.
-}
data Note = C | Cs | Db | D | Ds | Eb | E | F | Fs | Gb | G | Gs | Ab | A | As | Bb | B
  deriving (Show, Eq, Read)

{- |
MIDI Pitch, in range 0-128.
-}
type Pitch = Int -- Midi pitch (C4 = 60) (da cambiare in Word8?)


data Accident = Sharp | Flat deriving (Show, Eq)



-- ** Utility functions

mapEq :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapEq is f = map (\x -> if is x then f x else x)

-- | Rotates a list and applies a function to the piece of list pasted at the end.
rotateWith :: Ord a => (a -> a) -> Int -> [a] -> [a]
rotateWith func n xs = sort $ newStart ++ (map func newEnd) 
  where newEnd   = take n xs
        newStart = drop n xs


-- * Notes and intervals

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


{- |
Absolute distance between two MIDI pitches.
-}
distance :: Pitch -> Pitch -> Int
distance n1 n2 = abs $ n1 - n2


{- |
Given a `Pitch` and a list of `Interval`s, returns a list of pitches each
distant an `Interval` from the starting `Pitch`.

It is basically a map over a list of `Interval`s.
-}
line :: Pitch -> [Interval] -> [Pitch]
line note = map (jump note)


{- |
Alters an interval (eg. @11 -> 11#@, @9 -> 9b@...); for example:

>>> alter Fourth Sharp
Tritone

Mostly used for chords alterations.
-}
alter :: Interval -> Accident -> Interval
alter int Flat  = toEnum (fromEnum int - 1)
alter int Sharp = toEnum (fromEnum int + 1)



-- * Chords

-------------------------
-- Chords fundamentals --
-------------------------
data Chord = Chord [Interval] deriving (Eq, Show)

{- |
How many notes a chord has.
-}
numNotes :: Chord -> Int
numNotes (Chord is) = length is

{- |
Returns the n-th inversion of a chord.
-}
inversion :: Int -> Chord -> Chord
inversion num (Chord is) = Chord $ rotate num is
  where rotate n xs = take (length xs) . drop n . cycle $ xs



-- ** Combinators for creating chords.

{- |
 Adds an interval on top.
-}
addOnTop :: Interval -> Chord -> Chord
addOnTop i (Chord is) = Chord $ is ++ [i]

{- |
 Alters an interval in the chord.
-}
alterChord :: Interval -> Accident -> Chord -> Chord
alterChord i accident (Chord is) = Chord $ mapEq (== i) (flip alter accident) is 

{- |
 Removes an interval from the chord.
-}
remove :: Interval -> Chord -> Chord
remove interval (Chord is) = Chord $ filter (/= interval) is

{- |
 Sorts the chord. (useful?)
-}
sorted :: Chord -> Chord
sorted (Chord is) = Chord $ Data.List.sort is

{- |
 Combines chords combinators.
-}
(|>) :: (Chord -> Chord) -> (Chord -> Chord) -> Chord -> Chord
(|>) = flip (.)



-- ** Real Chords

majTriad = Chord [Unison, ThirdMaj, Fifth]
minTriad = Chord [Unison, ThirdMin, Fifth]

x_7    = addOnTop SeventhMin
x_maj7 = addOnTop SeventhMaj
x_9    = addOnTop NinthMaj
x_11   = addOnTop Eleventh
x_13   = addOnTop ThirteenthMaj
sus4   = remove ThirdMaj |> remove ThirdMin |> addOnTop Fourth |> sorted
soWhat = Chord [ThirdMaj, SixthMaj, NinthMaj, Twelfth, DoubleSeventhMaj]

{- |
 Realizes a chord, that is: given a Chord and a tonic, builds the Midi-pitch
representation of the chord.

>>> realizeChord majTriad 60
[60,64,67]

-}
realizeChord :: Chord -> Pitch -> [Pitch]
realizeChord (Chord is) tonic = line tonic is



-- * Sequences

-------------------------
-- Sequences of chords --
-------------------------

{- |
  A `Sequence` is a list of `Interval`s (the bass-line) and of `Chord`s.

  The bass-line has the form of a list of `Interval`; in a way, you could
describe a `Sequence` as a sequence of chords in a single tonality; or, also,
a sequence of chords that doesn't modulate.

  For every bass note at index i in `[Interval]`, the chord at index i of `[Chord]` will be built.

  An example of a ii-V-I sequence:

  >>> ii_v_i
  Sequence [Interval SecondMaj,Interval Fifth,Interval Unison]
           [Chord [Interval Unison,Interval ThirdMin,Interval Fifth,Interval SeventhMin],
            Chord [Interval Unison,Interval ThirdMaj,Interval Fifth,Interval SeventhMin],
            Chord [Interval Unison,Interval ThirdMaj,Interval Fifth,Interval SeventhMaj]]
-}
data Sequence = Sequence [Interval] [Chord] deriving (Show, Eq)

-- ** Sequences combinators

{- |
Remove the last index and chord of a sequence.
removeLastSeq :: Sequence -> Sequence
removeLastSeq (Sequence is cs) = Sequence (init is) (init cs)

{- |
Substitute every occurrence of @(Interval, Chord)@ with another tuple.
substitution :: (Interval, Chord) -> (Interval, Chord) -> Sequence -> Sequence
substitution old new (Sequence is cs) = Sequence (map fst subbed) (map snd subbed)
  where couples = zip is cs
        subbed  = mapEq (== old) (\_ -> new) couples

{- |
Tritone substitution.
tritoneSub :: Sequence -> Sequence
tritoneSub = substitution
             (Fifth, x_7 majTriad)
             (SecondMin, x_7 majTriad)

{- |
So What? substitution; re-harmonizes maj7 chords on @I@ with soWhat chords
soWhatSub :: Sequence -> Sequence
soWhatSub = substitution
            (Unison, x_maj7 majTriad)
            (Unison, soWhat)

(|>>|) :: (Sequence -> Sequence) -> (Sequence -> Sequence) -> Sequence -> Sequence
(|>>|) = (.)


{- |
  Realize a `Sequence`, that is: given a starting `Pitch`, build the chords on the intervals
relative to that midi pitch.

  For example:

  >>> realizeSeq 60 ii_v_i
  [[62,65,69,72],[67,71,74,77],[60,64,67,71]]

  Here, the @Interval Unison@ is set at pitch 60 (central C), so every interval is
calculated relative to 60 (eg. @Interval SecondMaj@ will be 62).
-}
realizeSeq :: Pitch -> Sequence -> [[Pitch]]
realizeSeq tonica (Sequence intervals chords) =
    zipWith (flip realizeChord) bassline chords
  where bassline = line tonica intervals

-- ** Shortest Sequence Realization
{- |
  Same as `realizeSeq`, but attempts to find the "nearest" substituions in the sequence.

  This means that, given two consecutive chords @C1@ and @C2@, @C2@ will be realized
with the inversion that minimizes the distance with @C1@.

  For example:

  >>> realizeSeqCompact 60 ii_v_i
  [[62,65,69,72],[62,65,67,71],[60,64,67,71]]

  Note: albeit being very easy to implement, this function still doesn't work properly.
-}
realizeSeqCompact :: Pitch -> Sequence -> [[Pitch]]
realizeSeqCompact tonica (Sequence intervals chords) = scanl1 shortestPathMidi realizedChords
  where bassline       = line tonica intervals
        realizedChords = zipWith (flip realizeChord) bassline chords

{- |
 All the inversions of a `Pitch`-chord
inversionMidi :: [Pitch] -> [[Pitch]]
inversionMidi ns = concat $ map -- use non-deterministic monad? >>= on list
                   (\rot -> [rotateWith id rot ns,
                             rotateWith (+12) rot ns,
                             map (subtract 12) $ rotateWith (+12) rot ns])
                   [0..length ns]

{- |
 Find the "shortest" chord among the inversions of @C2@, relative to @C1@.
shortestPathMidi :: [Pitch] -> [Pitch] -> [Pitch]
shortestPathMidi c1 c2 = inversions' !! indexMin
  where inversions' = inversionMidi c2
        ratedInversions = map (zipWith distance c1) inversions'
        indexMin  = fromJust $ elemIndex (minimum ratedInversions) ratedInversions


-- ** Some real sequences
v_i    = Sequence [Fifth, Unison]
                  [x_7 majTriad, x_maj7 majTriad]
ii_v_i = Sequence [SecondMaj, Fifth, Unison]
                  [x_7 minTriad, x_7 majTriad, x_maj7 majTriad]
ii_v_tritone = (removeLastSeq |>>| tritoneSub) ii_v_i



-- * Harmonies

{- |
  An `Harmony` is basically a collection of sequences, each of which has its own tonality.

  One big difference with `Sequence`s is that the @[Interval]@s are now relative to each other.
-}
data Harmony = Harmony [MelodicInterval] [Sequence] --tonalità - accordi
  deriving (Show, Eq)

{- |
A simple harmony with no modulation; it is basically a `Sequence`.
emptyHarmony :: Sequence -> Harmony
emptyHarmony seq = Harmony [Up Unison] [seq]

circleFifths :: Harmony
circleFifths = Harmony ints seqs
  where ints = replicate 12 (Down SecondMaj)
        seqs = replicate 12 ii_v_i

{- |
 Coltrane changes harmony.
coltrane :: Harmony
coltrane = Harmony ints seqs
  where ints = concat $ replicate 3 [Up ThirdMaj, Down SixthMin]
        seqs = replicate 3 v_i

{- |
 Realize an `Harmony`.
realizeHarmony :: Pitch -> Harmony -> [[Pitch]]
realizeHarmony note harmony = realizeHarmonyWith note realizeSeq harmony 

{- |
 Realize an `Harmony`, with the possibility to choose a custom sequence-realizing
-- function (eg. `realizeSeqCompact`).
realizeHarmonyWith :: Pitch -> (Pitch -> Sequence -> [[Pitch]]) -> Harmony -> [[Pitch]]
realizeHarmonyWith note seqRealizer (Harmony is seq) =
  concatMap (\(tonica, s) -> seqRealizer tonica s) zipped
  where bassline = scanl (\acc i -> jump acc i) note is
        zipped   = zip bassline seq


------------------------------------------------------------------------------------

-- ** Midi Exporting

{- |
 Creates a complete MIDI file given a `MidiTrack`.
createMidiFile :: MidiTrack -> MidiFile
createMidiFile track =
  MidiFile (MidiHeader MF0 1 (TPB 80))
           [track] --only 1 track!


data MidiOptions = MidiOptions
  { bpm :: Word32
  , shortestPath :: Maybe (Pitch -> Sequence -> [[Pitch]])
  , tuning :: Maybe HT.Temperament
}

{- |
 Given a list of chords (i.e. `[[Pitch]]`) returns a `MidiTrack`.
makeTrack :: MidiOptions -> [[Pitch]] -> MidiTrack
makeTrack (MidiOptions bpm mayShortest mayTuning) chords =
  MidiTrack $ (trackPreamble bpm mayTuning) ++ concatMap midiChorder chords

  where seqRealizer = if isNothing mayShortest then realizeSeq else realizeSeqCompact


trackPreamble :: Word32 -> Maybe HT.Temperament -> [MidiMessage]
trackPreamble bpm tuning = 
  [ (0,MetaEvent (SetTempo $ bpmToMidiTempo bpm))
  , (0,MetaEvent (TimeSignature 4 2 24 8))        -- 4/4
  , (0,VoiceEvent RS_OFF (ProgramChange 0 0))     -- instrument
  ] ++ tuningMessage
  
  where tuningMessage = if isNothing tuning 
                        then [(0, MetaEvent (TextEvent SEQUENCE_NAME "Standard tuning!"))]
                        -- else HT.changeTuningMessage (fromJust tuning)
                        else HT.splitTuningMessage $ HT.changeTuningMessage (fromJust tuning)


{- |
 Translates [Pitch] into MIDI messages.
midiChorder :: [Pitch] -> [MidiMessage]
midiChorder c = concat $ [notesOn, notesOff]
  where notesOn  = map (\pitch -> (0, VoiceEvent RS_OFF (NoteOn 0 (fromIntegral pitch) 100))) c
        notesOff = map (\pitch -> (64, VoiceEvent RS_OFF (NoteOff 0 (fromIntegral pitch) 0))) c



{- |
 BPM to MIDI tempo converter.
bpmToMidiTempo :: Word32 -> Word32
bpmToMidiTempo bpm = 60_000_000 `div` bpm







writeMidiFile :: String -> MidiFile -> IO ()
writeMidiFile = writeMidi



exampleHarmony = emptyHarmony $ Sequence [Unison] [remove Fifth majTriad]
examplePitches = realizeHarmonyWith (noteToMidi C) realizeSeq exampleHarmony
exampleTrack = makeTrack (MidiOptions 50 Nothing Nothing) examplePitches
exampleWrite = writeMidiFile "examples/chordsEq.mid" $ createMidiFile $ exampleTrack


      
--------------

