{-# LANGUAGE OverloadedStrings #-}
module IR
  ( IRWord(..)
  , IR(..)
  , Prim(..)
  , ArithOp(..)
  , StackOp(..)
  , ControlOp(..)
  , convert) where

import Parse
import qualified Data.Text as T

data IRWord = IRWord { irname :: T.Text
                     , ir  :: [IR]
                     }
  deriving(Show, Eq)

data IR = PushNum Int
        | PrimOp Prim
        | NamedWord T.Text
        | Emit T.Text
        deriving(Show, Eq)

data Prim = ArithOp ArithOp
            | StackOp StackOp
            | ControlOp ControlOp
            deriving(Show, Eq)

data ArithOp = Add | Sub | Mul | Div | And | Or | Xor
  deriving(Show, Eq)
data StackOp = Dup | Drop | Swap
  deriving(Show, Eq)
data ControlOp = Begin | Until | If | Else | Then
  deriving(Show, Eq)

arithmap :: [(T.Text, ArithOp)]
arithmap = [("+", Add), ("-", Sub), ("*", Mul), ("and", And), ("or",Or), ("xor", Xor)]
stackmap :: [(T.Text, StackOp)]
stackmap = [("dup", Dup), ("drop", Drop)]
controlmap :: [(T.Text, ControlOp)]
controlmap = [("begin", Begin), ("until", Until), ("if", If), ("else", Else), ("then", Then)]
prims =
  let arith = map (\(s,a) -> (s, ArithOp a)) arithmap
      stack = map (\(s,a) -> (s, StackOp a)) stackmap
      control = map (\(s,a) -> (s, ControlOp a)) controlmap
  in arith ++ stack ++ control


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
