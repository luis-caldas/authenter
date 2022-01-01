#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.random pkgs.random-shuffle ])"

-- vim: set tabstop=8 softtabstop=0 expandtab shiftwidth=8 smarttab:

-- Imports

import Data.List
import Text.Printf

import Debug.Trace

import System.Random (newStdGen, StdGen)
import System.Random.Shuffle (shuffle')


-- Functions

getSubList :: Int -> Int -> [Int] -> [Int]
getSubList start size inList =
        take sizeGood $ drop startGood inList
        where
                startGood = fromIntegral start
                sizeGood = fromIntegral size

shuffleSimpleFunction :: [Int] -> StdGen -> [Int]
shuffleSimpleFunction listIn rndGenerated =
        shuffle' listIn (length listIn) rndGenerated

splitIntoRows :: Int -> Int -> [Int] -> [[Int]]
splitIntoRows collumnSize rowSize listIn =
        map getSubNow [ 1 .. collumnSize ]
        where
                getSubNow :: Int -> [Int]
                getSubNow eachCollumn = getSubList (rowSize * (eachCollumn - 1)) rowSize listIn

interEdge :: a -> [a] -> [a]
interEdge character listIn =
        [character] ++ (intersperse character listIn) ++ [character]

generateTable :: Int -> Int -> [Int] -> String
generateTable width height scrambledList =
        concat $ interEdge fullSeparator $ map horizontalPrint markedMatrix
        where
                decimals = length $ show $ width * height
                numbersMatrix = splitIntoRows height width scrambledList

                fullSeparator = "\n " ++ (generateSeparator (length $ markedMatrix!!0)) ++ "\n"
                markedMatrix = map (\eachNr -> [eachNr] ++ (numbersMatrix!!eachNr)) intList
                        where
                                intList = [ 0 .. (length numbersMatrix) - 1 ]

                horizontalPrint :: [Int] -> String
                horizontalPrint listIn = concat $ interEdge " | " $ map (printf printfString) listIn
                        where
                                printfString = "%0" ++ (show decimals) ++ "d"

                generateSeparator :: Int -> String
                generateSeparator size = concat $ interEdge "|" $ map (\_ -> separator) [ 1 .. size ]
                        where
                                separatorChar = "-"
                                separator = concat $ map (\_ -> separatorChar) [ 1 .. ( decimals + 2 ) ]

-- Globals

tableWidth = 10
tableHeight = 10

main = do
        -- Verbose
        putStrLn "--- Authentication Table ---\n"

        -- Generate random
        rnd <- newStdGen

        -- Shuffle list
        let shuffledList = shuffleSimpleFunction listNow rnd
                where listNow = [ 0 .. ((tableWidth * tableHeight) - 1) ]

        -- Generate list
        let table = generateTable tableWidth tableHeight shuffledList

        -- Print everything
        putStrLn table
