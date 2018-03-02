module LowIR where
import IR
import qualified Data.Text as T
import Control.Monad.Writer
import Control.Monad.State
import Data.List (intersperse)

data LowWord = LowWord { name :: T.Text
                       , lowir :: [LIR]
                       }
instance Show LowWord where
  show LowWord{name=n, lowir=l} =
    let strs = map show l
    in  (T.unpack n) ++ ":\n\t" ++ (concat (intersperse "\n\t" strs)) ++ "\n"
data Register = R Int
              deriving (Show, Eq)

data Label = L Int
  deriving (Show, Eq)
data LIR = PushLit Int
         | PushR Register
         | Pop Register
         | Peek Register
         | Mov Register Register
         | Movl Register Int
         | Op ArithOp Register Register Register
         | Opl ArithOp Register Int
         | Label Label
         | Jmp Label 
         | JmpZ Label Register
         | Control ControlOp
         | Call T.Text
         | Emit T.Text
         deriving (Show, Eq)
                         
type Program = [LIR]

lower :: [IRWord] -> [LowWord]
lower irwords =
  let ws = map lowerDef irwords
  in evalState (resolveLabels ws) LS {stack=[], curLabel=0}
  where lowerDef d =
          let lir =  execWriter (lowerIrList (ir d)) 
          in LowWord{name=(irname d), lowir=lir}

lowerIrList :: [IR] -> Writer Program ()
lowerIrList (ir:rest) = do
  lowerIr ir
  lowerIrList rest
  

lowerIrList [] = return ()

emit :: LIR -> Writer Program ()
emit l = tell [l]

lowerIr :: IR -> Writer Program ()
lowerIr (PushNum x) = emit $ PushLit x
lowerIr (NamedWord s) = emit $ Call s
lowerIr (PrimOp (ArithOp op)) = do
  emit $ Pop (R 1)
  emit $ Pop (R 0)
  emit $ Op op (R 0) (R 0) (R 1)
  emit $ PushR (R 0)
lowerIr (PrimOp (StackOp Dup)) = do
  emit $ Peek (R 0)
  emit $ PushR (R 0)
lowerIr (PrimOp (StackOp Drop)) = do
  emit $ Pop (R 0)
lowerIr (IR.Emit t) = emit $ LowIR.Emit t
lowerIr (PrimOp (ControlOp op)) = emit $ Control op
lowerIr n = error $ "Now lowerIR for " ++ (show n)


data LabelState = LS { stack ::[(ControlOp, Label)]
                     , curLabel :: Int}


resolveLabels :: [LowWord] -> State LabelState [LowWord]
resolveLabels = mapM labelWord

labelWord :: LowWord -> State LabelState LowWord
labelWord w = do
  lir <- mapM labelNode (lowir w)
  let lir' = concat lir
  return $ w {lowir=lir'}

freshlabel :: State LabelState Label
freshlabel = do
  li <- state (\s -> (curLabel s, s {curLabel=(curLabel s)+1}))
  return $ L li

push :: (ControlOp,Label) -> State LabelState ()
push item =
  modify (\s -> s {stack=item:(stack s)})
pop :: State LabelState (ControlOp, Label)
pop =
  state (\ LS {stack=t:st, curLabel=cl} -> (t, LS {stack=st, curLabel=cl}))

labelNode :: LIR -> State LabelState [LIR]
labelNode (Control Begin) = do
  label <- freshlabel
  push (Begin, label)
  return [Label label]
labelNode (Control Until) = do
  (co, label) <- pop
  case co of
    Begin -> return [Pop (R 0), JmpZ label (R 0)]
    _ -> return $ error "Expecting to close a Begin"
  
labelNode x = return [x]
              
