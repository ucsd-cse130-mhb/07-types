
{-# LANGUAGE ScopedTypeVariables #-}

module Common where

import Data.IORef
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners
import System.Exit
import Control.Exception

type Score = IORef (Int, Int)

extraPoints :: Int
extraPoints = 15  -- update or remove this from assignment to assignment 

runTests :: [Score -> TestTree] -> IO ()
runTests groups = do
  sc <- initScore
  defaultMainWithIngredients (includingOptions coreOptions : defaultIngredients) 
    (tests sc groups) `catch` (\(e :: ExitCode) -> do
      (n, tot) <- readIORef sc
      let tot' = if tot > 0 then tot - (2 * extraPoints) else tot - extraPoints)
      putStrLn ("OVERALL SCORE = " ++ show n ++ " / "++ show tot')
      throwIO e)

tests :: Score -> [Score -> TestTree] -> TestTree
tests x gs = testGroup "Tests" [ g x | g <- gs ]

--------------------------------------------------------------------------------
-- | Construct a single test case
--------------------------------------------------------------------------------
mkTest' :: (Show b, Eq b) => Score -> (a -> IO b) -> a -> b -> String -> TestTree
--------------------------------------------------------------------------------
mkTest' sc f x r name = scoreTest' sc (f, x, r, 1, name)

--------------------------------------------------------------------------------
scoreTest' :: (Show b, Eq b) => Score -> (a -> IO b, a, b, Int, String) -> TestTree
--------------------------------------------------------------------------------
scoreTest' sc (f, x, expR, points, name) =
  testCase name $ do
    updateTotal sc points
    actR <- f x
    if actR == expR
      then updateCurrent sc points
      else assertFailure "Wrong Result"

updateTotal :: Score -> Int -> IO ()
updateTotal sc n = modifyIORef sc (\(x, y) -> (x, y + n))

updateCurrent :: Score -> Int -> IO ()
updateCurrent sc n = modifyIORef sc (\(x, y) -> (x + n, y))

initScore :: IO Score
initScore = newIORef (0, 0)
