{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Sudoku where

import Data.List
import Data.Maybe

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Fail
import Control.Monad.State

indices :: [Int] -> [a] -> [a]
indices = indices' . sort 
indices' :: [Int] -> [a] -> [a]
indices' [] _ = []
indices' _ [] = []
indices' l@(i:idxs) (x:xs) 
    | i == 0 = x : indices' (map (flip (-) 1) idxs) xs
    | otherwise = indices' (map (flip (-) 1) l) xs

exchange :: Int -> a -> [a] -> [a]
exchange _ _ [] = []
exchange 0 x (y:xs) = x:xs
exchange a x (y:ys) = y : exchange (a-1) x ys

unique :: (Eq a) => [a] -> Bool
unique l = length l == length (nub l)

instance Show (Maybe Int) where
    show Nothing = "_"
    show (Just s) = show s

data Coords = Coords {posx :: Int, posy :: Int}
newtype Sudoku = Sudoku {sudlist :: [[Maybe Int]]}

emptyBoard :: Sudoku
emptyBoard = Sudoku $ replicate 9 (replicate 9 Nothing)

runSudokerT (SudokerT s) = runStateT s
execSudokerT (SudokerT s) = execStateT s
evalSudokerT (SudokerT s) = evalStateT s

class (Monad m) => MonadSudoker m where
    valid :: m Bool
    putMaybe :: Coords -> Maybe Int -> m ()
    clearCoord :: Coords -> m ()
    putNumber :: Coords -> Int -> m ()
    possibilities :: Coords -> m [Int]
    allPossibilities :: m [(Coords, [Int])]
    board :: m Sudoku

    putNumber c n = putMaybe c (Just n)
    clearCoord c = putMaybe c Nothing

newtype SudokerT m a = SudokerT (StateT Sudoku m a) deriving (Functor, Applicative, Monad, MonadTrans, MonadState Sudoku, MonadIO)
newtype Sudoker a = Sudoker (SudokerT Identity a) deriving (Functor, Applicative, Monad, MonadState Sudoku)

number :: Coords -> Sudoku -> Maybe Int
number (Coords x y) (Sudoku ll) = (ll !! y) !! x

sudRow :: Int -> Sudoku -> [Maybe Int]
sudRow r (Sudoku ll) = ll !! r

sudCol :: Int -> Sudoku -> [Maybe Int]
sudCol c (Sudoku ll) = flip (!!) c <$> ll

sudSq :: Coords -> Sudoku -> [Maybe Int]
sudSq (Coords x y) (Sudoku ll) = 
    let sqx = x `div` 3
        sqy = y `div` 3 in
            concatMap (indices [3*sqx..3*sqx+2]) (indices [3*sqy..3*sqy+2] ll)

sudSqNum :: Int -> Sudoku -> [Maybe Int]
sudSqNum n = sudSq $ Coords (3*(n `mod` 3)) n

sudAllowed :: Coords -> Sudoku -> [Int]
sudAllowed pos@(Coords x y) s@(Sudoku ll) = (([1..9] \\ catMaybes (sudRow y s)) \\ catMaybes (sudCol x s)) \\ catMaybes (sudSq pos s)


instance (Monad m) => MonadSudoker (SudokerT m) where
    board = get
    putMaybe (Coords x y) m = do
                newl <- gets (exchange x m . flip (!!) y . sudlist)
                modify $ Sudoku . exchange y newl . sudlist
    valid = and <$> sequence
        [and <$> mapM (\e -> gets (unique . catMaybes . sudRow e)) [0..8],
         and <$> mapM (\e -> gets (unique . catMaybes . sudCol e)) [0..8],
         and <$> mapM (\e -> gets (unique . catMaybes . sudSqNum e)) [0..8],
         all (\(_, l) -> not $ null l) <$> allPossibilities]
    
    possibilities c = gets (sudAllowed c)
    allPossibilities = let all = uncurry Coords <$> ((,) <$> [0..8] <*> [0..8]) in
        sortOn (length . snd) <$> mapM (\c -> (,) c <$> possibilities c) all

instance (MonadFail m) => MonadFail (SudokerT m) where
    fail = fail



instance Show Sudoku where
    show (Sudoku ll) = replicate 25 '-' ++ "\n" ++ concatMap showRow' (zip [0..] ll)
        where showRow = concatMap (\(i, e) -> show e ++ (if i `mod` 3 == 2 then " | " else " "))
              showRow' (nrow, l) = "| " ++ showRow (zip [0..] l) ++ "\n" 
                ++ (if nrow `mod` 3 == 2 then replicate 25 '-' ++ "\n" else "")