module LowIR where
import IR
import qualified Data.Text as T
import Control.Monad.Writer
import Data.List (intersperse)

data LowWord = LowWord { name :: T.Text
                       , lowir :: [LIR]
                       }
instance Show LowWord where
  show LowWord{name=n, lowir=l} =
    let strs = map show l
    in  (T.unpack n) ++ ":\n\t" ++ (concat (intersperse "\n\t" strs)) ++ "\n"
data Register = R Int
              deriving (Show)
data LIR = PushLit Int
         | PushR Register
         | Pop Register
         | Op ArithOp Register Register Register
         | Call T.Text
         | Emit T.Text
         deriving (Show)
                         
type Program = [LIR]

lower :: [IRWord] -> [LowWord]
lower = map lowerDef
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
  emit $ Pop (R 0)
  emit $ Pop (R 1)
  emit $ Op op (R 0) (R 0) (R 1)
  emit $ PushR (R 0)
lowerIr (IR.Emit t) = emit $ LowIR.Emit t
lowerIr _ = tell []


