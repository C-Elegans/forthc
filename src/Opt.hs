{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}
module Opt where
import Control.Monad.State
import IR
import Data.List (union)
import qualified Data.Text as T
import Debug.Trace(trace)

data StackVal = Const Int
              | Bot
              deriving(Show)

data Stack = Stack{ stack :: [(StackVal, [Int])]
                  , ir ::[IR]
                  , deletions :: [Int]}
  deriving(Show)

tos :: [(StackVal, [Int])] -> (StackVal, [Int])
tos (s:_) = s
tos [] = (Bot, [])
push :: StackVal -> [Int] -> Opt ()
push l i = modify' (\s -> s{stack=(l,i):(stack s)})

clearstack :: Opt ()
clearstack = modify' (\s -> s{stack=[]})

type Opt = State Stack

opt :: [IRWord] -> [IRWord]
opt = runOpt . trim
  where runOpt = map (optIrWord)
  

rundeletions :: Int -> [Int] -> [IR] -> [IR]
rundeletions i dels (ir:rest) =
  case elem i dels of
    True -> rundeletions (i+1) dels rest
    False -> ir:(rundeletions (i+1) dels rest)
rundeletions _ _ [] = []

delete :: [Int] -> Opt ()
delete is = modify' (\s -> s{deletions=(deletions s)++is})

optIrWord :: IRWord -> IRWord
optIrWord iw =
  let state = Stack{stack=[], ir=[], deletions=[]}
      (_,st) = runState (optIR 0 (IR.ir iw)) state 
      optir = rundeletions 0 (deletions st) $ Opt.ir st
  in IRWord{irname=(irname iw), ir=optir}

optIR :: Int -> [IR] -> Opt [IR]
optIR i (w:ws) = do
  w' <- optword i w
  modify' (\s -> s{ir=(Opt.ir s) ++ [w']}::Stack)
  ws' <- optIR (i+1) ws
  return $ w':ws'
optIR _ [] = return $[]


optword :: Int -> IR -> Opt IR
optword i (PushNum x) = do
  
  modify' (\s -> trace (show s) s{stack=(Const x, [i]):(stack s)})
  return (PushNum x)
optword i p@(PrimOp (StackOp Dup)) = do
  s <- get
  case (tos (stack s)) of
    (Const x, _) -> do
      push (Const x) [i]
      return (PushNum x)
    _ -> return p
optword i p@(PrimOp (ArithOp Add)) = do
  s <- get
  case (stack s) of
    ((Const x1, i1):(Const x2, i2):rest) -> do
      let val = x1+x2
      push (Const val) [i]
      delete $ i1++i2
      return (PushNum val)
    _ -> do
      clearstack
      return p
optword i p@(PrimOp (ArithOp _)) = do
  s <- get
  case (stack s) of
    ((_, i1):(_, i2):rest) -> do
      push Bot $ [i] ++ i1 ++ i2
      return p
    _ -> do
      clearstack
      return p
optword i p@(PrimOp (StackOp Drop)) = do
  s <- get
  case (stack s) of
    (_, is):rest | not $ null is -> do
                   delete is
                   delete [i]
                   return p
    _ ->do
      clearstack
      return p
optword _ x = do
  clearstack
  return x


trim :: [IRWord] -> [IRWord]
trim words =
  let wordmap = trace (show words) $ map (\w -> (irname w, w)) words
      allused = usedWords "main" wordmap
  in allused

usedWords :: T.Text -> [(T.Text, IRWord)] -> [IRWord]
usedWords name words =
  case lookup name words of
    Nothing -> error $ (T.unpack name) ++ " not defined"
    Just word ->
      let used = map (getUsed words) (IR.ir word)
      in  word:(foldl1 (union) used)
                          
  where
    getUsed :: [(T.Text, IRWord)] -> IR -> [IRWord]
    getUsed words (NamedWord n) = usedWords n words
    getUsed words _ = []
    
