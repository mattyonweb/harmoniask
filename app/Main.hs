module Main where

import Harmoniask
import HarmoniaskParser as HP
import HarmoniaskTuning as HT
import Data.Word (Word32)
import Options.Applicative

data MyApp = MyApp { inputFile  :: String
                   , outputFile :: String
                   , bpm        :: Word32
                   , shortestPath :: Bool
                   , temperamentName :: HT.DefaultTuning}

runWithOptions :: MyApp -> IO ()
runWithOptions options =
  application (inputFile options)
              (outputFile options)
              (MidiOptions (Main.bpm options)
                            Nothing
                            temperament')
  where temperament' = HT.temperament $ temperamentName options

main :: IO ()
main = execParser options >>= runWithOptions
  where
    parser = MyApp <$>
          argument str (metavar "Input file" <> value "examples/input.txt")
      <*> argument str (metavar "Output file" <> value "examples/output.mid")
      <*> argument auto (metavar "BPM" <> value 80)
      <*> switch (short 's' <> long "shortest")
      <*> argument auto (metavar "Temperament")
      
    options = info parser mempty


----------------------------

application :: String -> String -> MidiOptions -> IO ()
application fnameIn fnameOut options = do
  rawInput <- readFile fnameIn
  
  let chords = map HP.stringToPitches $ words rawInput
  
  writeMidiFile fnameOut $
    createMidiFile $
      makeTrack options chords

