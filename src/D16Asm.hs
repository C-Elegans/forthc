{-# LANGUAGE OverloadedStrings #-}
module D16Asm where
import LowIR
import IR(ArithOp(..))
import qualified Data.Text as T
import Control.Monad.Writer
import Numeric (showHex)
import Data.Char


type Asm = Writer T.Text
       

assemble :: [LowWord] -> T.Text
assemble lws = execWriter $ assembleAll lws
  where
    assembleAll ::  [LowWord] -> Asm ()
    assembleAll lws = do
          tell initAsm
          mapM_ assembleWord lws

assembleWord :: LowWord -> Asm ()
assembleWord lw = do
  tell $ (escape (name lw)) <> ":\n"
  tell $ "\tpushlr\n"
  mapM_ assembleNode (lowir lw)
  tell $ "\tpop r1\n\tjmp r1\n"

assembleNode :: LIR -> Asm ()
assembleNode (Call t) = tell $ "\tcall " <> (escape t) <> "\n"
assembleNode (Emit t) = tell $ "\t" <> t <> "\n"
assembleNode (PushLit x) = tell $ "\tpush " <> (T.pack (show x)) <> ", r6" <> "\n"
assembleNode (Pop (R r)) = tell $ "\tpop r" <> (T.pack (show r)) <> ", r6" <> "\n"
assembleNode (PushR (R r)) = tell $ "\tpush r" <> (T.pack (show r)) <> ", r6" <> "\n"
assembleNode (Op Add (R r1) (R r2) (R r3))
  | r1 == r2 =  do
      tell $ "\tadd r" <> (T.pack (show r1)) <> ", r" <> (T.pack (show r3)) <> "\n"
assembleNode _ = return ()
  

escape :: T.Text -> T.Text
escape lw = "_f" <> T.concatMap escapechar lw 
  where escapechar c | isAlphaNum c = T.singleton c
        escapechar c =
          let num = ord c
          in  T.pack (showHex num "")
initAsm = "main:\n\
       \mov r7, 0x8000 \n\
       \mov r6, 0x7000 \n\
       \call " <> escape "main" <> "\n" <>
       "0:\n\
       \kill\n"

