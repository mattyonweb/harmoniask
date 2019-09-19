module Main where

import Harmoniask
import HarmoniaskParser as HP
import HarmoniaskTuning as HT
import Data.Word (Word32)
import Data.List (intercalate)
import Options.Applicative

data MyApp = MyApp { inputFile  :: String
                   , outputFile :: String
                   , bpm        :: Word32
                   , temperamentName :: HT.DefaultTuning
                   , shortestPath :: Bool }

main :: IO ()
main = execParser options >>= runHarmoniaskWithOptions
  where
    parser = MyApp
      <$> argument str (metavar "INPUT"
                       <> help "Input file; a textual document where chords are separated by spaces")

      <*> argument str (metavar "OUTPUT"
                       <> help "Output file; a midi file.")

      <*> argument auto (metavar "BPM"
                       <> value 80
                       <> help "Tempo of the output midi file." )

      <*> option auto (metavar "Temperament"
                       <> short 't'
                       <> long "temperament"
                       <> value HT.Equable
                       <> help
                          (intercalate ", " $ map show $ ([minBound..maxBound] :: [HT.DefaultTuning])))

      <*> switch (short 's'
                       <> long "shortest"
                       <> help "Not implemented yet.")
      
    options = info (parser <**> helper)
      (fullDesc <>
       progDesc "Given a textual file INPUT containing chords, returns an harmonized midi file OUTPUT" <>
       header "harmoniask - chords harmonizer")


----------------------------

runHarmoniaskWithOptions :: MyApp -> IO ()
runHarmoniaskWithOptions options =
  application (inputFile options)
              (outputFile options)
              (MidiOptions (Main.bpm options)
                            Nothing
                            temperament')
  where temperament' = HT.temperament $ temperamentName options
  
application :: String -> String -> MidiOptions -> IO ()
application fnameIn fnameOut options = do
  rawInput <- readFile fnameIn
  
  let chords = map HP.stringToPitches $ words rawInput
  
  writeMidiFile fnameOut $
    createMidiFile $
      makeTrack options chords

