{-# LANGUAGE OverloadedStrings #-}
module D16Asm where
import LowIR
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
  mapM_ assembleNode (lowir lw)

assembleNode :: LIR -> Asm ()
assembleNode (Call t) = tell $ "\tcall " <> (escape t) <> "\n"
assembleNode (Emit t) = tell $ "\t" <> t <> "\n"
assembleNode (PushLit x) = tell $ "\tpush " <> (T.pack (show x)) <> ", r6" <> "\n"
assembleNode _ = return ()
  

escape :: T.Text -> T.Text
escape lw = "_f" <> T.concatMap escapechar lw 
  where escapechar c | isAlphaNum c = T.singleton c
        escapechar c =
          let num = ord c
          in  T.pack (showHex num "")
initAsm = "mov r7, 0x8000 \n\
       \mov r6, 0x7000 \n\
       \call " <> escape "main" <> "\n"
