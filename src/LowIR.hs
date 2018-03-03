module LowIR where
import IR
import qualified Data.Text as T
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Map as M
import Debug.Trace (trace)
import Data.List (intersperse)

data LowWord = LowWord { name :: T.Text
                       , lowir :: [LIR]
                       , attributes :: M.Map T.Text Attr
                       }
instance Eq LowWord where
  lw1 == lw2 = (name lw1) == (name lw2) && (lowir lw1) == (lowir lw2)

data Attr = Arity Int Int
          | StackOnly
          | Const
          | NoAsm
          deriving (Eq)

instance Show LowWord where
  show LowWord{name=n, lowir=l} =
    let strs = map show l
    in  (T.unpack n) ++ ":\n\t" ++ (concat (intersperse "\n\t" strs)) ++ "\n"
data Register = R Int
              deriving (Show, Eq, Ord, Read)

data Label = L Int
  deriving (Show, Eq, Read)
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
         deriving (Show, Eq, Read)
                         
type Program = [LIR]

lower :: [IRWord] -> [LowWord]
lower irwords =
  let ws = map lowerDef irwords
  in evalState (resolveLabels ws) LS {stack=[], curLabel=0}
  where lowerDef d =
          let lir =  execWriter (lowerIrList (ir d)) 
          in LowWord{name=(irname d), lowir=lir, attributes=M.fromList []}

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
lowerIr (IR.EmitLow t) =
  let lir = trace (show t) $ read (T.unpack t) :: [LIR]
  in tell $ lir
lowerIr (PrimOp (ControlOp op)) = emit $ Control op
lowerIr n = error $ "No lowerIR for " ++ (show n)


data LabelState = LS { stack ::[(ControlOp, Label)]
                     , curLabel :: Int}


resolveLabels :: [LowWord] -> State LabelState [LowWord]
resolveLabels = mapM labelWord

labelWord :: LowWord -> State LabelState LowWord
labelWord w = do
  loop <- mapM labelLoop (lowir w)
  let loop' = concat loop
  ifs <- mapM labelIf (reverse loop')
  let ifs' = concat $ reverse ifs
  return $ w {lowir=ifs'}

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

labelLoop :: LIR -> State LabelState [LIR]
labelLoop (Control Begin) = do
  label <- freshlabel
  push (Begin, label)
  return [Label label]
labelLoop (Control Until) = do
  (co, label) <- pop
  case co of
    Begin -> return [Pop (R 0), JmpZ label (R 0)]
    _ -> return $ error "Expecting to close a Begin"
  
labelLoop x = return [x]
              
labelIf :: LIR -> State LabelState [LIR]
labelIf (Control Then) = do
  label <- freshlabel
  push (Then, label)
  return [Label label]
labelIf (Control If) = do
  (co, label) <- pop
  case co of
    Then -> return [Pop (R 0), JmpZ label (R 0)]
    Else -> do
      (co2, thenlabel) <- pop
      case co2 of
        Then -> return [Pop (R 0), JmpZ label (R 0)]
        _ -> error $ "Mismatched If Else Then"
    _ -> error $ "Mismatched If Then"
labelIf (Control Else) = do
  (co, label) <- pop
  case co of
    Then -> do
      mylabel <- freshlabel
      push (co, label)
      push (Else, mylabel)
      return [Jmp label, Label mylabel]
    _ -> error $ "Mismatched If Then"
labelIf l = return [l]
