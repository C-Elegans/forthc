{-# LANGUAGE OverloadedStrings #-}
module IR
  ( IRWord(..)
  , IR(..)
  , Prim(..)
  , ArithOp(..)
  , StackOp(..)
  , convert) where

import Parse
import qualified Data.Text as T

data IRWord = IRWord { irname :: T.Text
                     , ir  :: [IR]
                     }
  deriving(Show)

data IR = PushNum Int
        | PrimOp Prim
        | NamedWord T.Text
        | Emit T.Text
        deriving(Show)

data Prim = ArithOp ArithOp
            | StackOp StackOp
            deriving(Show)

data ArithOp = Add | Sub | Mul | Div 
  deriving(Show)
data StackOp = Dup | Drop | Swap
  deriving(Show)

arithmap :: [(T.Text, ArithOp)]
arithmap = [("+", Add), ("-", Sub)]
stackmap :: [(T.Text, StackOp)]
stackmap = [("dup", Dup), ("drop", Drop)]
prims =
  let arith = map (\(s,a) -> (s, ArithOp a)) arithmap
      stack = map (\(s,a) -> (s, StackOp a)) stackmap
  in arith ++ stack 


convert :: [WordDef] -> [IRWord]
convert = map convertDef
  where convertDef wd = IRWord{irname=(name wd), ir=(convertWords (Parse.words wd))}

convertWords :: [Token] -> [IR]
convertWords = map convertWord

convertWord :: Token -> IR
convertWord (Number n) = PushNum n
convertWord (Parse.Emit t) = IR.Emit t
convertWord (Word t) =
  case lookup t prims of
    Just prim -> PrimOp prim
    _ -> NamedWord t
