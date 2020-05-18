{-# LANGUAGE TemplateHaskell #-}

module Counter_9 (counter_9_Sys) where

import ForSyDe.Deep
import Data.Int


-- Input: Run
-- 'H' : Count
-- 'L' : Hold
type Run = Bit

nextStateFun :: ProcFun (Int8 -> Run -> Int8)
nextStateFun = $(newProcFun
   [d| nextState state dir = if dir == H then
                                 if state < 9 then
                                     state + 1
                                 else
                                     0
                             else
                               state
     |])

outputFun :: ProcFun (Int8 -> (Run, Int8))
outputFun = $(newProcFun
   [d| output state = if state == 9 then
                        (H, state)
                      else
                        (L, state)
     |])


counterProc :: Signal Run -> Signal (Run, Int8) 
counterProc = mooreSY "counterProc" nextStateFun outputFun 0


counter_9_Sys :: SysDef (Signal Run -> Signal (Run, Int8))
counter_9_Sys = newSysDef counterProc "Counter_9" ["direction"] ["run", "number"]
