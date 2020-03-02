module Sudoku where

import Data.List
import Data.Maybe

indices :: [Int] -> [a] -> [a]
indices = indices' . sort 
indices' :: [Int] -> [a] -> [a]
indices' [] _ = []
indices' _ [] = []
indices' l@(i:idxs) (x:xs) 
    | i == 0 = x : indices' (map (flip (-) 1) idxs) xs
    | otherwise = indices' (map (flip (-) 1) l) xs

data Coords = Coords {posx :: Int, posy :: Int}
newtype Sudoku = Sudoku {sudlist :: [[Maybe Int]]}

number :: Sudoku -> Coords -> Maybe Int
number (Sudoku ll) (Coords x y) = (ll !! y) !! x

sudRow :: Sudoku -> Int -> [Maybe Int]
sudRow (Sudoku ll) r = ll !! r

sudCol :: Sudoku -> Int -> [Maybe Int]
sudCol (Sudoku ll) c = flip (!!) c <$> ll

sudSq :: Sudoku -> Coords -> [Maybe Int]
sudSq (Sudoku ll) (Coords x y) = 
    let sqx = x `div` 3
        sqy = y `div` 3 in
            concatMap (indices [3*sqx, 3*sqx+2]) (indices [3*sqy..3*sqy+2] ll)

sudAllowed :: Sudoku -> Coords -> [Int]
sudAllowed s@(Sudoku ll) pos@(Coords x y) = (([1..9] \\ catMaybes (sudRow s y)) \\ catMaybes (sudCol s x)) \\ catMaybes (sudSq s pos)