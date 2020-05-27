{-# LANGUAGE TemplateHaskell #-}

module SevenSegmentDecoder (sevenSegDecSys) where

import ForSyDe.Deep
import ForSyDe.Deep.Bit
import Data.Param.FSVec
import Data.Int
import Data.TypeLevel.Num.Reps
import Data.TypeLevel.Num.Aliases

-- L turns LED on

decodeFun :: ProcFun (Int8 -> FSVec D7 Bit)
decodeFun 
   = $(newProcFun [d|decode :: Int8 -> FSVec D7 Bit
                     decode x 
                       = case x of 
                           0 -> H +> L +> L +> L +> L +> L +> L +> empty
                           1 -> H +> H +> H +> H +> L +> L +> H +> empty
                           2 -> L +> H +> L +> L +> H +> L +> L +> empty
                           3 -> L +> H +> H +> L +> L +> L +> L +> empty
                           4 -> L +> L +> H +> H +> L +> L +> H +> empty
                           5 -> L +> L +> H +> L +> L +> H +> L +> empty
                           6 -> L +> L +> L +> L +> L +> H +> L +> empty
                           7 -> H +> H +> H +> H +> L +> L +> L +> empty
                           8 -> L +> L +> L +> L +> L +> L +> L +> empty
                           9 -> L +> L +> H +> L +> L +> L +> L +> empty
                           _ -> H +> H +> H +> H +> H +> H +> H +> empty
                              |])

sevenSegDecProc :: Signal Int8 -> Signal (FSVec D7 Bit)
sevenSegDecProc = mapSY "decode" decodeFun

sevenSegDecSys :: SysDef (Signal Int8 -> Signal (FSVec D7 Bit))
sevenSegDecSys = newSysDef sevenSegDecProc "sevenSegDec" ["in"] ["out"] 

-- ==> Simulation with Modelsim
simulateModelsim :: [Int8] -> IO [FSVec D7 Bit]
simulateModelsim = writeAndModelsimVHDL Nothing sevenSegDecSys

-- ==> Hardware Generation

-- IMPORTANT: Programming the DE10 Standard:
-- > quartus_pgm -c DE-SoC -m JTAG -o "p;./sevenSegDec/vhdl/sevenSegDec.sof@2"
generateHW_DE_10_Standard :: IO ()
generateHW_DE_10_Standard = writeVHDLOps vhdlOps sevenSegDecSys
 where vhdlOps = defaultVHDLOps{execQuartus=Just quartusOps}
       quartusOps = QuartusOps{action=FullCompilation,
                               fMax=Just 24, -- in MHz
                               fpgaFamiliyDevice=Just ("Cyclone V",
                                                       Just "5CSXFC6D6F31C6"),
                               -- Possibility for Pin Assignments
                               pinAssigs=[("in[0]", "PIN_AB30"),  -- SW0
                                          ("in[1]", "PIN_Y27"),   -- SW1
                                          ("in[2]", "PIN_AB28"),  -- SW2
                                          ("in[3]", "PIN_AC30"),  -- SW3
                                          ("in[4]", "PIN_W25"),   -- SW4
                                          ("in[5]", "PIN_V25"),   -- SW5
                                          ("in[6]", "PIN_AC28"),  -- SW6
                                          ("in[7]", "PIN_AD30"),  -- SW7
                                          ("out[6]","PIN_W17"),   -- HEX0[0]
                                          ("out[5]","PIN_V18"),   -- HEX0[1]
                                          ("out[4]","PIN_AG17"),  -- HEX0[2]
                                          ("out[3]","PIN_AG16"),  -- HEX0[3]
                                          ("out[2]","PIN_AH17"),  -- HEX0[4]
                                          ("out[1]","PIN_AG18"),  -- HEX0[5]
                                          ("out[0]","PIN_AH18")   -- HEX0[6]
                                         ]
                              }
