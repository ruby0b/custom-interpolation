{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Char (toUpper)
import QQ
import System.Exit (exitFailure, exitSuccess)

test :: String -> String -> IO Bool
test actual expected = do
  putStrLn $ "-- Expecting: \"" <> expected <> "\""
  putStrLn actual
  if expected == actual
    then do
      putStrLn "-- ^ Test passed.\n"
      return True
    else do
      putStrLn "-- ^ ERROR: NOT EQUAL!\n"
      return False

runTests :: [(String, String)] -> IO ()
runTests tests = do
  results <- mapM (uncurry test) tests
  if and results
    then exitSuccess
    else exitFailure

main :: IO ()
main = do
  runTests
    [ ( [itest|Escaped dollar sign: \$|],
        "Escaped dollar sign: $"
      ),
      ( [itest|Quoted expression containing an escaped closing brace evaluating to: ${case () of {() -> map toUpper ("hello world" ++ ['!'])}}|],
        "Quoted expression containing an escaped closing brace evaluating to: HELLO WORLD!"
      ),
      ( [itest|Quoted expressions evaluating to: ${map toUpper "hello"} and ++{41 :: Int}|],
        "Quoted expressions evaluating to: HELLO and 42"
      ),
      let x = 42.3 :: Double
       in ( [iManyBrackets|$<"<" <> "<" <> ">" <> ['>'] <> "<>>"> $(3 * (x - 6) + read "(7)") [[1, 2 ..] :: [Int]] {"hello"}|],
            "<<>><>> " ++ show (3 * (x - 6) + 7) ++ " [1,2,3,4] HELLO"
          ),
      -- readme examples
      ( [i|2^10 = {show (2 ^ 10)}. Some Fibonacci numbers: @{let fibs = 1 : 1 : zipWith (+) fibs (tail fibs) in fibs}.|],
        "2^10 = 1024. Some Fibonacci numbers: [1,1,2,3,5,8,13,21,34,55]."
      ),
      ( show [sql|SELECT * FROM user WHERE id = {(11 ^ 5)} AND lastName = {"Smith"}|],
        "(\"SELECT * FROM user WHERE id = ? AND lastName = ?\",[161051,\"Smith\"])"
      )
    ]
