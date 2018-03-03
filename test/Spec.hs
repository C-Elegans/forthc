{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit hiding (State)
import OptLowSpec

  
main :: IO ()
main = do
  runTestTT $ TestList $ testOptLow
  return ()

