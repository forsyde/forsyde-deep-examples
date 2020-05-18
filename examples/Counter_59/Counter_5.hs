{-# LANGUAGE TemplateHaskell #-}

module Counter_5 (counter_5_Sys) where

import ForSyDe.Deep
import Data.Int

type Run = Bit -- 'H' : Count
               -- 'L' : Hold 

nextStateFun :: ProcFun (Int8 -> Run -> Int8)
nextStateFun = $(newProcFun
   [d| nextState state dir = if dir == H then
                                 if state < 5 then
                                     state + 1
                                 else
                                     0
                             else
                               state
     |])

outputFun :: ProcFun (Int8 -> (Run, Int8))
outputFun = $(newProcFun
   [d| output state = if state == 5 then
                        (H, state)
                      else
                        (L, state)
     |])


counterProc :: Signal Run -> Signal (Run, Int8) 
counterProc = mooreSY "counterProc" nextStateFun outputFun 0


counter_5_Sys :: SysDef (Signal Run -> Signal (Run, Int8))
counter_5_Sys = newSysDef counterProc "Counter_5" ["direction"] ["run", "number"]
