{-# LANGUAGE LambdaCase #-}

module Lib where

import Sudoku
import Data.Maybe

import Control.Monad
import Control.Monad.Except
import Control.Monad.State

type SudMon a = SudokerT (ExceptT String IO) a

runPlay :: IO ()
runPlay = print emptyBoard >> runExceptT (evalSudokerT play emptyBoard) >>= \case
    Left err -> putStrLn err
    Right _ -> return ()

play :: SudMon ()
play = do
    [x,y,n] <- map read . words <$> liftIO getLine
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