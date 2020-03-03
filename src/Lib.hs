{-# LANGUAGE LambdaCase #-}

module Lib where

import Sudoku
import Data.Maybe

import Control.Monad
import Control.Monad.Except
import Control.Monad.State

type SudIO = SudokerT IO 

read' :: String -> Maybe Int
read' s = let r = read s in if r `elem` [1..9] then Just r else Nothing

runPlay :: IO ()
runPlay = putStrLn "Hello to Sudoku solver!\n" >> print emptyBoard >> evalSudokerT play emptyBoard >>= \case
    Nothing -> putStrLn "Impossible to solve"
    Just sud -> putStrLn "Solved:\n" >> print sud

play :: SudIO (Maybe Sudoku)
play = do
    c:comm <- words <$> liftIO getLine
    if c == "solve" then solve
    else if c == "input" then
        replicateM 9 (map read' . words <$> liftIO getLine) >>= put . Sudoku >> solve
    else do 
        let [x,y,n] = map read (c:comm)
        putNumber (Coords x y) n
        valid >>= \case
            False -> do
                liftIO (putStrLn "Invalid number!")            
                clearCoord (Coords x y)
                (row, col, sq) <- (,,) <$> gets (catMaybes . sudRow y) <*> gets (catMaybes . sudCol x) <*> gets (catMaybes . sudSq (Coords x y))
                liftIO $ putStrLn $ "In row: " ++ show row
                liftIO $ putStrLn $ "In column: " ++ show col
                liftIO $ putStrLn $ "In square: " ++ show sq
            True -> board >>= liftIO . print
        play

tryNumbers :: (Coords, [Int]) -> SudIO (Maybe Sudoku)
tryNumbers (_, []) = pure Nothing
tryNumbers (c,(x:xs)) = do
    putNumber c x
    solve >>= \case
        Nothing -> clearCoord c >> tryNumbers (c,xs)
        Just sud -> return $ Just sud

solve :: SudIO (Maybe Sudoku)
solve = do
    poss <- allPossibilities
    if null poss then return <$> board else tryNumbers (head poss)