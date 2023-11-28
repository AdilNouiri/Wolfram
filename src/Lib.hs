{-- 
-- EPITECH PROJECT, 2022
-- wolfram
-- File description:
-- Lib.hs
--}

module Lib
    (   
        Conf(..),
        defaultConf,
        myElem,
        isInt,
        getOpts,
        intToBinaryReversed,
        generate,
        createFirstLine,
        checkNbrLines
    ) where

import Data.Maybe (isJust)
import Data.Char (isDigit)
import Data.List (find)

data Conf = Conf {
    rule :: Maybe Int,
    start :: Int,
    line :: Maybe Int,
    window :: Int,
    move :: Maybe Int
} deriving (Eq, Ord, Show)

defaultConf :: Conf
defaultConf = Conf {
    rule = Nothing,
    start = 0,
    line = Nothing,
    window = 80,
    move = Nothing
}

myElem :: Eq a => a -> [a] -> Bool
myElem nbr = isJust.find (== nbr)

isInt :: String -> String -> Bool
isInt [] _ = False
isInt ('-':_) "--move" = True
isInt list _
    | myElem False (map isDigit list) = False
    | otherwise = True

getOpts :: Conf -> [String] -> Maybe Conf
getOpts conf@Conf{rule = Nothing} [] = Nothing
getOpts conf [] = Just conf
getOpts _ (x1:x2:_)
    | not (isInt x2 x1) = Nothing
getOpts conf ("--rule":x2:xs)
    | read x2 <= 256 = getOpts (conf {rule = Just (read x2)}) xs
getOpts conf ("--start":x2:xs) = getOpts (conf {start = read x2}) xs
getOpts conf ("--lines":x2:xs) = getOpts (conf {line = Just (read x2)}) xs
getOpts conf ("--window":x2:xs) = getOpts (conf {window = read x2}) xs
getOpts conf ("--move":x2:xs) = getOpts (conf {move = Just (read x2)}) xs
getOpts _ _ = Nothing

intToBinaryReversed :: Int -> String
intToBinaryReversed x = reverse $ pad (reverse $ intToBinaryReversed' x "")
    where
        intToBinaryReversed' 0 acc = acc
        intToBinaryReversed' n acc =
            intToBinaryReversed' (n `div` 2) (acc ++ show (n `mod` 2))
        pad str = replicate (8 - length str) '0' ++ str

selectStar :: Char -> Char
selectStar '1' = '*'
selectStar '0' = ' '
selectStar _ = error "Le caractère n'est pas 0 ou 1"

binaryToInt :: String -> Int
binaryToInt [a,b,c] = 4 * read [a] + 2 * read [b] + read [c]
binaryToInt _ = 0

charToBinary :: Char -> Char -> Char -> String
charToBinary c1 c2 c3 = [if c == '*' then '1' else '0' | c <- [c1, c2, c3]]

checkNbrLines :: Maybe Int -> Int
checkNbrLines Nothing = -1
checkNbrLines (Just nbrLines) = nbrLines

transformCharacter :: String -> Int -> String
transformCharacter table index = [selectStar (table !! index)]

createLine :: String -> String -> Int -> String
createLine _ prevLine index
    | index >= (length prevLine - 1) = ""
createLine table prevLine index =
    transformCharacter table
        (binaryToInt (charToBinary (prevLine !! (index - 1))
            (prevLine !! index) (prevLine !! (index + 1)))) ++
                createLine table prevLine (index + 1)

myPutStrLn :: String -> (Int, Int) -> IO ()
myPutStrLn lineToPrint (x, y) = putStrLn (take (y + x) (drop (-x) lineToPrint))

createFirstLine :: Int -> Int -> String
createFirstLine size nbrMove =
    let halfSize = size `div` 2
        leftPadding = replicate (halfSize + nbrMove) ' '
        rightPadding = if odd size
            then replicate (halfSize - nbrMove) ' '
            else replicate (halfSize - 1 - nbrMove) ' '
    in leftPadding ++ "*" ++ rightPadding

generate :: String -> Int -> Int -> (Int, Int) -> String -> IO()
generate _ _ 0 _ _ = putStr ""
generate table 0 nbrLines (x, y) nbrLine = myPutStrLn nbrLine (x, y)
    >> generate table 0 (nbrLines - 1) (x - 1, y + 1)
        (createLine table (concat ["  ", nbrLine, "  "]) 1)
generate table startValue nbrLines (x, y) nbrLine =
    generate table (startValue - 1) nbrLines (x - 1, y + 1)
        (createLine table (concat ["  ", nbrLine, "  "]) 1)
