{-- 
-- EPITECH PROJECT, 2022
-- wolfram
-- File description:
-- Main.hs
--}

module Main (main) where
import System.Environment
import System.Exit
import Data.Maybe (fromMaybe)

import Lib
    (   
        Conf(..),
        defaultConf,
        getOpts,
        intToBinaryReversed,
        generate,
        createFirstLine,
        checkNbrLines
    )

main :: IO ()
main =
    getArgs >>= \args ->
    case getOpts defaultConf args of
        Just conf ->
            let table = maybe "00000000" intToBinaryReversed(rule conf)
            in generate table (start conf) (checkNbrLines (line conf))
                (0, window conf) (createFirstLine (window conf)
                    (fromMaybe 0 (move conf))) >> exitSuccess
        Nothing -> putStrLn "Invalid arguments" >> exitWith (ExitFailure 84)
