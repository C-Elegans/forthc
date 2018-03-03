{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit hiding (State)
import Control.Monad.State
import qualified Data.Text as T
import Opt hiding (push, stack)
import OptLow
import IR
import LowIR hiding (stack, push, pop)
import Parse
import qualified Data.Map as M
import Debug.Trace (trace)

testDataOptLow :: [[LIR]]
testDataOptLow =
  [ [PushLit 0, Peek (R 0), PushR (R 0)]
  , [Movl (R 0) 1, PushR (R 0), Pop (R 0)]
  , [Movl (R 0) 2, PushR (R 0), Pop (R 1)]
  ]


testOptLow d = TestCase $
  let w = [LowWord {LowIR.name="main", lowir=d}]
      u = runStackEngine $ w
      o = runStackEngine $ optlow $ w
  in  trace ("\n" ++ show o ++ "\n" ++ show u) $ assertEqual "unopt == opt" u o

tests n (d:rest) =
  let name = "test" ++ (show n)
      test = testOptLow d
      rest' = tests (n+1) rest
  in test:rest'
tests _ [] = []
  
main :: IO ()
main = do
  runTestTT $ TestList $ tests 1 testDataOptLow
  return ()


data StackState = SS { stack :: [Int]
                     , registers :: M.Map Register Int
                     }
  deriving (Show, Eq)
                
type StackEngine = State StackState

runStackEngine :: [LowWord] -> StackState
runStackEngine [LowWord{LowIR.name=_, lowir=lir}] =
  let (s, a) = runState ((mapM_ stackEngine) lir) SS
        {Main.stack=[], registers=M.fromList []}
  in  a

push :: Int -> StackEngine ()
push a = modify' (\s -> s{Main.stack=a:(Main.stack s)} :: StackState)

pop :: StackEngine (Maybe Int)
pop = state (\s ->
               case Main.stack s of
                 t:r -> (Just t, s{Main.stack=r})
                 _ -> (Nothing, s)
            )
setreg :: Register -> Int -> StackEngine ()
setreg r i =
  modify (\s -> s {registers=M.insert r i (registers s)})
getreg :: Register -> StackEngine (Maybe Int)
getreg r =
  get >>= \s ->
  return (M.lookup r (registers s))

stackEngine :: LIR -> StackEngine ()
stackEngine (PushLit l) = push l
stackEngine (Peek r) = do
  lit <- pop
  case lit of
    Just l -> do
      push l
      setreg r l
    Nothing -> return ()
stackEngine (PushR r) = do
  lit <- getreg r
  case lit of
    Just l -> push l
    Nothing -> error "empty register for PushR"
stackEngine (Pop r) = do
  lit <- pop
  case lit of
    Just l -> setreg r l
    Nothing -> return ()
stackEngine (Movl r l) =
  setreg r l
stackEngine (Mov r1 r2) = do
  lit <- getreg r2
  case lit of
    Just l -> setreg r1 l
    Nothing -> error $ "Register " ++ (show r2) ++ "empty"
                         
  

stackEngine n = error $ "No stackEngine defined for " ++ (show n)
  
  
  
