{-# LANGUAGE TemplateHaskell #-}

-- Example is based on Figure 2.1 of Rajeev Alur: Principles of Cyber-Physical Systems, MIT Press, 2015

module Delay (Button, delaySys) where

import ForSyDe.Deep
--import Data.Int

type Button = Bool

nextStateFun :: ProcFun (Bool -> Bool -> Bool)
nextStateFun = $(newProcFun
   [d| nextState state button = button
     |])

delayProc :: Signal Bool -> Signal Bool
delayProc = scanldSY "DelayProc" nextStateFun False

delaySys :: SysDef (Signal Bool -> Signal Bool)
delaySys = newSysDef delayProc "DelaySys" ["input"] ["output"]

-- Hardware Generation
-- IMPORTANT: Programming the DE10 Standard:
-- > quartus_pgm -c DE-SoC -m JTAG -o "p;./system/vhdl/system.sof@2"
compileQuartus_DelaySystem :: IO ()
compileQuartus_DelaySystem = writeVHDLOps vhdlOps delaySys
 where vhdlOps = defaultVHDLOps{execQuartus=Just quartusOps}
       quartusOps = QuartusOps{action=FullCompilation,
                               fMax=Just 50, -- in MHz
                               fpgaFamiliyDevice=Just ("Cyclone V",
                                                       Just "5CSXFC6D6F31C6"),
                               -- Possibility for Pin Assignments
                               pinAssigs=[("input", "PIN_AB30"),  -- SW0
                                          ("resetn", "PIN_Y27"),  -- SW1
                                          ("clock","PIN_AJ4"),    -- KEY[0]
                                          -- ("out[6]","PIN_W17"),   -- HEX0[0]
                                          -- ("out[5]","PIN_V18"),   -- HEX0[1]
                                          -- ("out[4]","PIN_AG17"),  -- HEX0[2]
                                          -- ("out[3]","PIN_AG16"),  -- HEX0[3]
                                          -- ("out[2]","PIN_AH17"),  -- HEX0[4]
                                          -- ("out[1]","PIN_AG18"),  -- HEX0[5]
                                          -- ("out[0]","PIN_AH18"),  -- HEX0[6]
                                          ("output", "PIN_AA24")   -- LEDR[0]
                                         ]
                              }
