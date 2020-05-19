{-# LANGUAGE TemplateHaskell, DeriveDataTypeable  #-}

-- Example is based on Figure 2.2 of Rajeev Alur: Principles of Cyber-Physical Systems, MIT Press, 2015

module Switch where

import ForSyDe.Deep
import Language.Haskell.TH.Lift (deriveLift1)
import Data.Generics (Data,Typeable)
import Data.Int

data State = OFF | ON deriving (Eq, Typeable, Data, Show)

$(deriveLift1 ''State)

type Press = Bool

nextStateFun :: ProcFun ((State, Int8) -> Bool -> (State, Int8))
nextStateFun = $(newProcFun
   [d| nextState (state, x) press = if state == OFF then
                                       if press == False then
                                         (OFF, 0)
                                       else
                                         (ON, 0)
                                    else
                                       if press == False && x < 5 then
                                         (ON, x+1)
                                       else
                                         (OFF,0)
     |])

outputFun :: ProcFun ((State, Int8) -> Bool)
outputFun = $(newProcFun
   [d| output (state, x) = if state == OFF then
                              False
                           else
                              True
     |])

switchProc :: Signal Bool -> Signal Bool
switchProc = mooreSY "SwitchProc" nextStateFun outputFun (OFF, 0)

switchSys :: SysDef (Signal Bool -> Signal Bool)
switchSys = newSysDef switchProc "SwitchSys" ["input"] ["output"]


-- ==> Simulation <==
testInput :: [Bool]
testInput =  [False, True, True, True, False, False, False, False, False, False, False, True, False]

-- ==> Simulation in ForSyDe
testSimulationForSyDe :: [Bool]
testSimulationForSyDe = simulate switchSys testInput

-- ==> Simulation with ModelSim
testSimulationModelsim :: IO [Bool]
testSimulationModelsim = writeAndModelsimVHDL Nothing switchSys testInput

-- ==> Hardware Generation

-- IMPORTANT: Programming the DE10 Standard:
-- > quartus_pgm -c DE-SoC -m JTAG -o "p;./SwitchSys/vhdl/SwitchSys.sof@2"
generateHW_DE_10_Standard :: IO ()
generateHW_DE_10_Standard = writeVHDLOps vhdlOps switchSys
 where vhdlOps = defaultVHDLOps{execQuartus=Just quartusOps}
       quartusOps = QuartusOps{action=FullCompilation,
                               fMax=Just 24, -- in MHz
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

