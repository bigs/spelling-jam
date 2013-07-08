module Main where

import Data.List (words)
import qualified Data.Map as M
import qualified Data.Set as S

type FrequencyMap = M.Map String Integer
type Word = String
type Edit = String
type Edits = S.Set Edit

train :: [Word] -> FrequencyMap

nWords :: Word -> FrequencyMap

alphabet :: [Char]
alphabet = ['a'..'z']

edits1 :: Word -> Edits

knownEdits2 :: Word -> Edits

known :: Word -> Edits

correct :: Word -> Edit

main :: IO ()
main = do
    putStr "word> "
    word <- getLine
    putStrLn . correct $ word

