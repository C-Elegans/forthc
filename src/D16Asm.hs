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
  tell $ (escape (name lw)) <> ":\t\t\t ;" <> (name lw) <> "\n"
  case (lowir lw) of
    [Emit t] -> assembleNode (Emit t)
    _ -> do
      tell $ "\tpushlr\n"
      mapM_ assembleNode (lowir lw)
      tell $ "\tpop r1\n\tjmp r1\n"

reg :: Register -> T.Text
reg (R r) = "r" <> lit r

lit :: Int -> T.Text
lit = T.pack . show

opStr :: ArithOp -> T.Text
opStr Add = "add"
opStr Sub = "sub"
opStr And = "and"
opStr Or = "or"
opStr Xor = "xor"

fromLabel :: Label -> T.Text
fromLabel (L l) = "_Z" <> lit l

assembleNode :: LIR -> Asm ()
assembleNode (Call t) = tell $ "\tcall " <> (escape t) <> "\n"
assembleNode (Emit t) = tell $ t <> "\n"
assembleNode (PushLit x) = tell $ "\tpush " <> (lit x) <> ", r6" <> "\n"
assembleNode (Pop r) = tell $ "\tpop " <> (reg r) <> ", r6" <> "\n"
assembleNode (Peek r) = tell $ "\tld " <> (reg r) <> ", [r6]\n"
assembleNode (PushR r) = tell $ "\tpush " <> (reg r) <> ", r6" <> "\n"
assembleNode (Label l) = tell $ fromLabel l <> ":\n"
assembleNode (JmpZ l r) = do
  tell $ "\ttest " <> (reg r) <> ", " <> (reg r) <> "\n"
  tell $ "\tjmp.eq " <> fromLabel l <> "\n"
assembleNode (Op Mul r1 r2 r3)
  | r1 == (R 0) && r2 == r1 && r3 == (R 1) =
      tell $ "\tcall _mul\n"
assembleNode (Op op r1 r2 r3)
  | r1 == r2 =  do
      tell $ "\t"<> (opStr op) <> " " <> (reg r1) <> ", " <> (reg r3) <> "\n"
assembleNode (Mov r1 r2) =
  tell $ "\tmov " <> (reg r1) <> ", " <> (reg r2) <> "\n"

assembleNode (Movl r1 l) =
  tell $ "\tmov " <> (reg r1) <> ", " <> (lit l) <> "\n"
assembleNode n = error $ "No assembleNode defined for " ++ (show n)
  

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

