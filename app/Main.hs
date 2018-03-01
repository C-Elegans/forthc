module Main where
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Parse (parse)
import IR
import Opt
import LowIR
import D16Asm

main :: IO ()
main = do
  args <- getArgs
  case args of
    [f, o] -> do
      x <- T.readFile f
      case parse x of
        Right ws -> do
          let ir = lower $ convert ws
          mapM_ print ir
          let asm = assemble ir
          T.putStrLn asm
          T.writeFile o asm 
        Left err -> putStrLn $ "Parse error: " ++ err
    _ -> putStrLn "usage: [forth.fs]"
