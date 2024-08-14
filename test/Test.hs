{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad (forever)
import Data.List (intercalate, isPrefixOf)
import System.Console.ANSI
  ( Color (Green, Red)
  , ColorIntensity (Vivid)
  , ConsoleLayer (Foreground)
  , SGR (Reset, SetColor)
  , clearScreen
  , setCursorPosition
  , setSGR
  )
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure)
import System.FSNotify (Event (Modified), watchTree, withManager)
import System.IO (hFlush, stdout)
import Test.DocTest (doctest)

removePrefix :: String -> String -> String
removePrefix p s
  | p `isPrefixOf` s = drop (length p) s
  | otherwise = s

isModified :: Event -> Bool
isModified Modified{} = True
isModified _ = False

tryExit :: a -> IO a -> IO (Either Int a)
tryExit a = fmap (either f Right) . try
 where
  f ExitSuccess = Right a
  f (ExitFailure c) = Left c

test :: [String] -> IO ()
test files = do
  clearScreen *> setCursorPosition 0 0
  putStrLn $ "Running tests for " <> intercalate ", " files <> "…"
  tryExit () (doctest files)
    >>= ( (\(c, s) -> setSGR [SetColor Foreground Vivid c] *> putStrLn s)
            . either (const (Red, "Tests failed!")) (const (Green, "Tests passed!"))
        )
  setSGR [Reset] *> hFlush stdout

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> test ["src/Part1.hs", "src/Part2.hs", "src/Part3.hs"]
    ("1" : _) -> test ["src/Part1.hs"]
    ("2" : _) -> test ["src/Part2.hs"]
    ("3" : _) -> test ["src/Part3.hs"]
    ("bonus" : _) -> test ["src/Bonus.hs"]
    _ -> do
      putStrLn $ "Invalid arguments " <> unwords args <> ".\nValid arguments: 1, 2, 3, bonus"
      exitFailure
  cwd <- getCurrentDirectory
  withManager $ \mgr -> do
    _ <- watchTree mgr "src" isModified $ \case
      Modified f _ _ -> test [removePrefix (cwd <> "/") f]
      _ -> pure ()
    forever (threadDelay 1000000)
