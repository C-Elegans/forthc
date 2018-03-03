{-# LANGUAGE OverloadedStrings #-}
module OptLow ( optlow )
  where
import Control.Monad.State hiding (fix)
import qualified Data.Map as M
import qualified Data.Text as T
import LowIR

optlow ir =
  let first = map optdef ir
      annotated = map annotate first
      inlined = inlineAll annotated
  in inlined

optdef :: LowWord -> LowWord
optdef w = w {lowir = fix (lowir w)}

fix ir =
  let o = optir ir
  in if o == ir then
       o
     else
       fix o
    
     

optir :: [LIR] -> [LIR]
optir ((PushR r1):(Pop r2):rest) | r1==r2 = optir rest
optir ((PushR r1):(Pop r2):rest) = (Mov r2 r1):optir rest

optir ((PushLit l):(Pop r):rest) = (Movl r l):optir rest
optir ((Pop r1):(PushR r2):rest) | r1==r2 = (Peek r1):rest
optir (l:ls) = l:(optir ls)
optir [] = []

annotate lw =
  let stonly = mapfrom stackonly "stackonly" StackOnly lw
      asm = mapfrom noasm "noasm" NoAsm lw
      mp = (attributes lw) `M.union` stonly `M.union` asm
  in lw {attributes=mp}
  where
    satisfies f lw = ((foldl (&&) True) . (map f)) (lowir lw)
    mapfrom f str at lw = if satisfies f lw then
                            M.fromList [(str, at)]
                          else
                            M.empty
    stackonly (PushLit _) = True
    stackonly (Pop _) = True
    stackonly (Peek _)= True
    stackonly (PushR _) = True
    stackonly _ = False
    noasm (Emit _) = False
    noasm _ = True

type Inline = State InlineState
type InlineState = M.Map T.Text LowWord

shouldInline :: M.Map T.Text Attr -> Bool
shouldInline _ = False

inlineAll :: [LowWord] -> [LowWord]
inlineAll lws =
  let mp = M.fromList $ map (\s -> (name s, s)) lws
  in  map (\l -> evalState (runInline l) mp) lws

runInline :: LowWord -> Inline LowWord
runInline lw = do
  ir <- inlineIr (lowir lw)
  return $ lw {lowir=ir}

inlineIr :: [LIR] -> Inline [LIR]
inlineIr ir = 
  mapM inline ir >>=
  return . concat

inline x = return [x]

  
  

