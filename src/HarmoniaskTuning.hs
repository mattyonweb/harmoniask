{-# LANGUAGE RankNTypes #-}
{-|
Description : Alternate Tunings
License     : GPL-3
Maintainer  : martinlutero@stronzi.org
Stability   : experimental

This module contains way to achieve alternative tunings via Midi Standard Tuning (MST).
-}

module HarmoniaskTuning
-- (
--   -- * Types
--   Temperament,
--   Freqs,
  
--   -- * Temperaments
--   DefaultTuning,
--   temperament,
--   eqHz,
--   pytHz,
--   werck1Hz,
--   strangeHz,
  
--   -- * Midi
--   changeTuningMessage,
--   splitTuningMessage
--   )
where


import GHC.Word
import ZMidi.Core

-- |The A0 frequency. 
a0 :: Floating a => a
a0 = 27.50

{- |
  Type synonym for a function that takes the frequency (in Hz) of the tonic and
  returns a list of the frequencies of the 12 notes within an octave from the tonic.
-}
type Temperament a = a -> [a]

{- |
  Type synonym for a map with form:
    MidiPitch : frequency in hz
-}
type Freqs a = [(Word8, a)]

{- |
  Takes a `Temperament` function and returns the frequencies of all the 88 notes of the piano,
  starting from A0 up to A7.
-}
pianoRange :: Floating a => Temperament a -> Freqs a 
pianoRange temperamentFunc = zip [21..] (concat scales)
  where octaves = take 7 $ iterate (*2) a0
        scales  = map temperamentFunc octaves


data DefaultTuning = Equable | Pythagoric | Werckmeister | Strange
  deriving (Show, Eq, Read)
temperament :: Floating a => DefaultTuning -> Maybe (Temperament a)
temperament Equable = Nothing
temperament Pythagoric = Just pytHz 
temperament Werckmeister = Just werck1Hz
temperament Strange = Just strangeHz

-- | Equable temperament.
eqHz :: Floating a => Temperament a
eqHz base = freqs
  where start = 2 ** (1/12)
        freqs = fmap (base *) (take 12 $ iterate (* start) 1)

-- |Pythagoric temperament.
pytHz :: Floating a => Temperament a
pytHz base = freqs
  where freqs = fmap (base *)
                [1, 256/243, 9/8, 32/27, 81/64, 4/3, 729/512, 3/2, 128/81, 27/16, 16/9, 243/128]

-- |Werckmeister I (III) temperament.
werck1Hz :: Floating a => Temperament a
werck1Hz base = freqs
  where root n x = x ** (1 / n)
        freqs = fmap (base *)
                [1, 256/243, (root 2 2) * 64/81, 32/27,
                 (root 4 2) * 256/243, 4/3, 1024/729,
                 (root 4 8) * 8/9, 128/81, (root 4 2) * 1024/729,
                 16/9, (root 4 2) * 128/81]

strangeHz :: Floating a => Temperament a
strangeHz base = freqs
  where freqs = fmap (base *) $
                [1] ++ map (\n -> (fromIntegral n) / 12) [13..23]
  
-- |Distance (in cents) between two frequencies.
freqsToCents :: Floating a => a -> a -> a
freqsToCents a b = 1200 * logBase 2 (a / b)

{- |
  Returns the representation used by Midi Standard Tuning (MST) to indicate an offset (in cents)
  from a base note.

  The list returned will be of length 2: `[MSB, LSB]`. 
-}
midiFineTune :: (RealFrac a, Floating a) => a -> [Word8]
midiFineTune 0 = [127, 127]
midiFineTune c = [high, if low == 127 then 126 else low]
  where high = truncate $ c / (0.78125)
        low  = truncate $ (c - (fromIntegral high * 0.78125)) / 0.0061


transform :: (RealFrac a, Floating a, Ord a) => (Word8, a) -> (Word8, Word8, a)
transform (i, diff')
  | diff >= 100  = (i, i + (diff `div` 100), fromIntegral $ diff `rem` 100)
  | diff >= 0    = (i, i, fromIntegral diff)
  | diff >= -100 = (i, i-1, fromIntegral $ 100 + diff)
  | otherwise    = (i, i + (diff `div` 100) + 1, fromIntegral $ 100 - ((abs diff) `rem` 100)) -- non ci sono offset-cents negativi in MST 

  where diff = truncate diff'
  
{- |
  Turns a Freqs into a MST-compatible format.
-}
makeMap :: (Ord a, Floating a, RealFrac a) => Freqs a -> [[Word8]]
makeMap freqs = map (\(p1,p2,diff) -> [p1, p2] ++ midiFineTune diff) diffs
  where equableTempFreqs = pianoRange eqHz

        -- temp :: (Word8, a) -> (Word8, a) -> (Word8, Word8, a)
        temp (i, x) (_, y) = transform (i, freqsToCents x y)

        -- differenze in cents tra l'equabile e il nuovo temperamento
        diffs = zipWith temp equableTempFreqs freqs --[(Word8, Word8, a)] 
 

{- |
  Given a new temperament, generates a `MidiMessage` that establishes a new tuning in
the midi file.
-}
changeTuningMessage :: Temperament Float -> MidiMessage
changeTuningMessage temperament = (0, SysExEvent (SysExSingle 0xF7 list))
  where freqs    = pianoRange temperament
        rawMap   = makeMap freqs
        numNotes = fromIntegral (length rawMap) :: GHC.Word.Word8
        list     = [0x7F,8,2,numNotes] ++ (concat rawMap) ++ [247]

chunks :: Int -> [a] -> [[a]]
chunks len xs
  | length xs >= len = [take 4 xs] ++ chunks len (drop 4 xs)
  | otherwise        = [xs]


fromMidiNotes :: MidiMessage -> [[Word8]]
fromMidiNotes (_, SysExEvent (SysExSingle _ list)) =
  chunks 4 $ init $ drop 4 list
  
splitTuningMessage :: MidiMessage -> [MidiMessage]
splitTuningMessage mex@(oo, SysExEvent (SysExSingle ooo list)) =
  map mexMaker $ notesGroups
  where numNotes = list !! 4
        notesGroups' = (chunks 4 $ fromMidiNotes mex) :: [[[Word8]]]
        notesGroups = if last notesGroups' == [[]]
                      then init notesGroups'
                      else if (last $ last notesGroups') == []
                           then init notesGroups' ++ [init $ last notesGroups']
                           else notesGroups'
        
        preamble :: [[Word8]] -> [Word8]
        preamble notes = [0x7F,8,2,fromIntegral $ length notes]

        mexMaker :: [[Word8]] -> MidiMessage
        mexMaker notes = (oo, SysExEvent
                           (SysExSingle ooo (preamble notes ++ (concat notes ++ [247]))))

-- changeTuningMessage :: Temperament Float -> MidiMessage
-- changeTuningMessage temperament = (0, SysExEvent (SysExSingle 23 list))
--   where freqs    = pianoRange temperament
--         rawMap   = makeMap freqs
--         numNotes = fromIntegral (length rawMap) :: GHC.Word.Word8
--         list     = [127,0,8,2,0,numNotes] ++ (concat rawMap) ++ [247]

-- chunks :: Int -> [a] -> [[a]]
-- chunks len xs
--   | length xs >= len = [take 4 xs] ++ chunks len (drop 4 xs)
--   | otherwise        = [xs]


-- fromMidiNotes :: MidiMessage -> [[Word8]]
-- fromMidiNotes (_, SysExEvent (SysExSingle _ list)) =
--   chunks 4 $ init $ drop 6 list
  
-- splitTuningMessage :: MidiMessage -> [MidiMessage]
-- splitTuningMessage mex@(oo, SysExEvent (SysExSingle ooo list)) =
--   map mexMaker $ notesGroups
--   where numNotes = list !! 5
--         notesGroups' = (chunks 4 $ fromMidiNotes mex) :: [[[Word8]]]
--         notesGroups = if last notesGroups' == [[]]
--                       then init notesGroups'
--                       else if (last $ last notesGroups') == []
--                            then init notesGroups' ++ [init $ last notesGroups']
--                            else notesGroups'
        
--         preamble :: [[Word8]] -> [Word8]
--         preamble notes = [127,0,8,2,0,fromIntegral $ length notes]

--         mexMaker :: [[Word8]] -> MidiMessage
--         mexMaker notes = (oo, SysExEvent
--                            (SysExSingle ooo (preamble notes ++ (concat notes ++ [247]))))
