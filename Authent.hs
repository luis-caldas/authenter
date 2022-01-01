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
        (intersperse character listIn) ++ [character]

generateTable :: Int -> Int -> [Int] -> String
generateTable width height scrambledList =
        concat $ interEdge fullSeparator $ map horizontalPrint markedMatrix
        where
                decimals = length $ show $ (width * height) - 1
                numbersMatrix = splitIntoRows height width scrambledList

                printfIndexString = "%" ++ (show decimals) ++ "d"
                nPrintfIndex = printf printfIndexString

                leftFullMatrix = map (\eachNr -> [nPrintfIndex eachNr] ++ (map nPrintf $ numbersMatrix!!eachNr)) intList
                        where
                                intList = [ 0 .. (length numbersMatrix) - 1 ]

                                printfString = "%0" ++ (show decimals) ++ "d"
                                nPrintf = printf printfString

                markedMatrix = [ customFirstLine ] ++ leftFullMatrix
                        where
                                emptySpace = concat $ map (\_ -> " ") [ 0 .. decimals - 1]
                                customFirstLine = [ emptySpace ] ++ (map nPrintfIndex [ 0 .. (length $ numbersMatrix!!0) - 1 ])

                fullSeparator = "\n " ++ (generateSeparator (length $ markedMatrix!!0)) ++ "\n"

                horizontalPrint :: [String] -> String
                horizontalPrint listIn = concat $ [ "  " ] ++ (interEdge " | " listIn)

                generateSeparator :: Int -> String
                generateSeparator size = concat $ interEdge "|" $ map (\_ -> separator) [ 1 .. size ]
                        where
                                separatorChar = "-"
                                separator = concat $ map (\_ -> separatorChar) [ 1 .. decimals + 2 ]

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
