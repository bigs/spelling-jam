module Main where

import Data.List (words, splitAt)
import qualified Data.Map as M
import qualified Data.Set as S
import System.IO (hFlush, stdout)
import Control.Monad (forever)

type FrequencyMap = M.Map String Integer
type Word = String
type Edit = String
type WordPart = String
type Edits = S.Set Word

train :: [Word] -> FrequencyMap
train words = M.fromListWith (+) oneTuples
    where oneTuples = [(word, 1) | word <- words]

alphabet :: [Char]
alphabet = ['a'..'z']

edits1 :: Word -> Edits
edits1 word = S.fromList $ concat [deletes, transposes, replaces, inserts]
    where s = [splitAt i word | i <- [0..length word]]
          deletes = [a ++ tail b | (a, b) <- s, not $ null b]
          transposes = [a ++ [b !! 1] ++ [b !! 0] ++ drop 2 b | (a, b) <- s, not $ null $ drop 1 b]
          replaces = [a ++ [c] ++ drop 1 b | (a, b) <- s, c <- alphabet, not $ null b]
          inserts = [a ++ [c] ++ b | (a, b) <- s, c <- alphabet]

knownEdits2 :: Word -> Edits
knownEdits2 word = S.fromList $ concatMap S.toList edits
    where edits = [edits1 e1 | e1 <- (S.toList $ edits1 word)]

nWords text = train . words $ text

known :: FrequencyMap -> Edits -> Edits
known wordMap words = S.fromList $ [word | word <- S.toList words, word `M.member` wordMap ]

--correct :: FrequencyMap -> Word -> Edit
correct wordMap word = M.maxView . M.filterWithKey (\k v -> k `S.member` candidates) $ wordMap
    where candidates = S.unions [known wordMap $ S.fromList [word] , 
                                 edits1 word, 
                                 knownEdits2 word, 
                                 S.fromList [word]]

main :: IO ()
main = do
    bigData <- readFile "big.txt"
    let bigDataNWords = nWords bigData
    forever $ do
        putStr "word> "
        hFlush stdout
        word <- getLine
        putStrLn . show . correct bigDataNWords $ word
    return ()
 
