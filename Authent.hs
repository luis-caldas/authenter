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

-- Gets a sub list from a list
getSubList :: Int -> Int -> [Int] -> [Int]
getSubList start size inList =
        take sizeGood $ drop startGood inList
        where
                startGood = fromIntegral start
                sizeGood = fromIntegral size

-- Simple shuffle function
shuffleSimpleFunction :: [Int] -> StdGen -> [Int]
shuffleSimpleFunction listIn rndGenerated =
        shuffle' listIn (length listIn) rndGenerated

-- Transforms a list into a two dimentional matrix given its dimentions
splitIntoRows :: Int -> Int -> [Int] -> [[Int]]
splitIntoRows collumnSize rowSize listIn =
        map getSubNow [ 1 .. collumnSize ]
        where
                getSubNow :: Int -> [Int]
                getSubNow eachCollumn = getSubList (rowSize * (eachCollumn - 1)) rowSize listIn

-- Intersperse with edges
interEdge :: a -> [a] -> [a]
interEdge character listIn =
        (intersperse character listIn) ++ [character]

-- Generate full table
generateTable :: Int -> Int -> [Int] -> String
generateTable width height scrambledList =
        -- Create the full table
        concat $ interEdge fullSeparator $ map horizontalPrint markedMatrix
        where
                -- Calculate the number of maximum decimals
                decimals = length $ show $ (width * height) - 1
                -- Generate the matrix from the shuffled list
                numbersMatrix = splitIntoRows height width scrambledList

                -- Create functions to help the printing of offsetted numbers
                printfString = "%0" ++ (show decimals) ++ "d"
                nPrintf = printf printfString
                printfIndexString = "%" ++ (show decimals) ++ "d"
                nPrintfIndex = printf printfIndexString

                -- Create the full matrix with marker numbers (indexes) on the left side
                leftFullMatrix = map (\eachNr -> [nPrintfIndex eachNr] ++ (map nPrintf $ numbersMatrix!!eachNr)) intList
                        where
                                intList = [ 0 .. (length numbersMatrix) - 1 ]

                -- Create the full matrix with the top row being the marker numbers (indexes)
                markedMatrix = [ customFirstLine ] ++ leftFullMatrix
                        where
                                emptySpace = concat $ map (\_ -> " ") [ 0 .. decimals - 1]
                                customFirstLine = [ emptySpace ] ++ (map nPrintfIndex [ 0 .. (length $ numbersMatrix!!0) - 1 ])

                -- Create a full line separator
                fullSeparator = "\n " ++ (generateSeparator (length $ markedMatrix!!0)) ++ "\n"

                -- Populate the line with proper data and spacing
                horizontalPrint :: [String] -> String
                horizontalPrint listIn = concat $ [ "  " ] ++ (interEdge " | " listIn)

                -- Function to generate a full line separator (non populated)
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
