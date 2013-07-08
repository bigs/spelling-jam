module Main where

import Data.List (words, splitAt, sortBy)
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

nWords :: String -> FrequencyMap
nWords text = train . words $ text

known :: FrequencyMap -> Edits -> Edits
known wordMap words = S.fromList $ [word | word <- S.toList words, M.member word wordMap]

correct :: (Edits -> Edits) -> Word -> [Word]
correct knownFn word = case (S.toList $ knownFn $ S.fromList [word]) of
    [] -> case (S.toList $ knownFn $ edits1 word) of
        [] -> case (S.toList $ knownEdits2 word) of
            [] -> [word]
            x -> x
        x -> x
    x -> x

compareTuple (_, i) (_, j) = if i < j
                             then LT
                             else if i == j
                             then EQ
                             else GT

orderResults :: FrequencyMap -> [Word] -> [Word]
orderResults freqMap results = map (\(x, _) -> x) $ reverse $ sortBy compareTuple $ map (\x -> (x, M.findWithDefault 1 x freqMap)) results

main :: IO ()
main = do
    bigData <- readFile "data/big.txt"
    let bigDataNWords = nWords bigData
    let known' = known bigDataNWords
    forever $ do
        putStr "word> "
        hFlush stdout
        word <- getLine
        putStrLn . show $ orderResults bigDataNWords $ correct known' word
    return ()
    --putStrLn . correct $ word

