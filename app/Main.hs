module Main where
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Parse (parse)
import IR
import Opt
import LowIR
import D16Asm
import OptLow

corefile = "core/core.fs"
main :: IO ()
main = do
  args <- getArgs
  case args of
    [f, o] -> do
      x <- T.readFile f
      core <- T.readFile corefile
      let inp = core `T.append` x
      case parse inp of
        Right ws -> do
          let ir = lower $ convert ws
          mapM_ print ir
          let optir = optlow ir
          mapM_ print optir
          let asm = assemble optir
          T.putStrLn asm
          T.writeFile o asm 
        Left err -> putStrLn $ "Parse error: " ++ err
    _ -> putStrLn "usage: [forth.fs]"
