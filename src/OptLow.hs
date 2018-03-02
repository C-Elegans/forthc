module OptLow ( optlow )
  where
import LowIR

optlow = map optdef

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
optir (l:ls) = l:(optir ls)
optir [] = []
