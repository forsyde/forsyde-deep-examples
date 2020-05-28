{-# LANGUAGE TemplateHaskell #-}

-- Minimal Example for Combinational Circuit: 2-input AND-GATE

module AND2 (and2Sys) where

import ForSyDe.Deep
import Data.Bits

and2Fun :: ProcFun (Bit -> Bit -> Bit)
and2Fun = $(newProcFun
   [d| and2 a b = a .&. b 
     |])

and2Proc :: Signal Bit -> Signal Bit -> Signal Bit
and2Proc = zipWithSY "and2" and2Fun

and2Sys :: SysDef (Signal Bit -> Signal Bit -> Signal Bit)
and2Sys = newSysDef and2Proc "And2Sys" ["a", "b"] ["y"]


-- ==> Simulation <==
testSig1 :: [Bit]
testSig1 = [L,L,H,H]
testSig2 :: [Bit]
testSig2 = [L,H,L,H]

-- ==> Simulation in ForSyDe
testSimulationForSyDe :: [Bit]
testSimulationForSyDe = simulate and2Sys testSig1 testSig2

-- ==> Simulation with ModelSim
testSimulationModelsim :: IO [Bit]
testSimulationModelsim = writeAndModelsimVHDL Nothing and2Sys testSig1 testSig2

-- ==> Hardware Generation

-- IMPORTANT: Programming the DE10 Standard:
-- > quartus_pgm -c DE-SoC -m JTAG -o "p;./And2Sys/vhdl/And2Sys.sof@2"
generateHW_DE_10_Standard :: IO ()
generateHW_DE_10_Standard = writeVHDLOps vhdlOps and2Sys
 where vhdlOps = defaultVHDLOps{execQuartus=Just quartusOps}
       quartusOps = QuartusOps{action=FullCompilation,
                               fMax=Just 24, -- in MHz
                               fpgaFamiliyDevice=Just ("Cyclone V",
                                                       Just "5CSXFC6D6F31C6"),
                               -- Possibility for Pin Assignments
                               pinAssigs=[("a", "PIN_AB30"),  -- SW0
                                          ("b", "PIN_Y27"),  -- SW1
                                          -- ("clock","PIN_AJ4"),    -- KEY[0]
                                          -- ("out[6]","PIN_W17"),   -- HEX0[0]
                                          -- ("out[5]","PIN_V18"),   -- HEX0[1]
                                          -- ("out[4]","PIN_AG17"),  -- HEX0[2]
                                          -- ("out[3]","PIN_AG16"),  -- HEX0[3]
                                          -- ("out[2]","PIN_AH17"),  -- HEX0[4]
                                          -- ("out[1]","PIN_AG18"),  -- HEX0[5]
                                          -- ("out[0]","PIN_AH18"),  -- HEX0[6]
                                          ("y", "PIN_AA24")   -- LEDR[0]
                                         ]
                              }
                             

