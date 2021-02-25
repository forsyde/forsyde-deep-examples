{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module VendingMachine_Moore where

import ForSyDe.Deep
import Data.Int
import Language.Haskell.TH.Lift
import Data.Generics (Data, Typeable)

data Coin = C5 | C10 deriving (Data, Typeable, Show, Eq, Ord)
data Bottle = B deriving (Data, Typeable, Show, Eq, Ord)
data Return = R deriving (Data, Typeable, Show, Eq, Ord)
data State = S0 | S5 | S10 | S15 deriving (Data, Typeable, Show, Eq, Ord)

$(deriveLift1 ''Coin)
$(deriveLift1 ''Bottle)
$(deriveLift1 ''Return)
$(deriveLift1 ''State)

type Coin_Event = AbstExt Coin
type Bottle_Event = AbstExt Bottle
type Return_Event = AbstExt Return

outputFun :: ProcFun (   State -- current State
                      -> (Bottle_Event, Return_Event)) -- (Bottle, Return)
outputFun =
  $(newProcFun
     [d| output :: State -> (Bottle_Event, Return_Event)
         output state = if (state == S0) || (state == S5) then
                          (Abst, Abst)
                        else
                          if state == S10 then
                            (Prst B, Abst)
                          else -- state == S15
                            (Prst B, Prst R)
                 |])

nextstateFun :: ProcFun (   State       -- current state (S0, S5, S10, S15)
                         -> Coin_Event  -- Coin
                         -> State)      -- next state
nextstateFun
  = $(newProcFun
       [d| nextstate :: State -> Coin_Event -> State
           nextstate state coin = if coin == Abst then
                                    if state == S10 || state == S15 then
                                      S0
                                    else
                                      state
                                  else
                                    if coin == Prst C5 then
                                      if state /= S5 then
                                        S5
                                      else
                                        S10
                                    else -- coin == C10
                                        if state /= S5 then
                                          S10
                                        else
                                          S15
         |])

vendingMachineProc :: Signal Coin_Event -- Signal of Coins
                   -> Signal (Bottle_Event, Return_Event)
vendingMachineProc = mooreSY "VendingMachine" nextstateFun outputFun S0

vendingMachineSys :: SysDef (Signal Coin_Event -- Signal of Coins
                     -> Signal (Bottle_Event, Return_Event))
vendingMachineSys = newSysDef vendingMachineProc "VendingMachine" ["coin"] ["bottle"]


-- ==> Simulation <==
testSig :: [Coin_Event]
testSig = [Prst C10, Prst C5, Prst C5, Prst C5, Prst C10, Abst] 

-- ==> Simulation in ForSyDe
testSimulationForSyDe :: [(Bottle_Event, Return_Event)]
testSimulationForSyDe = simulate vendingMachineSys testSig

-- ==> Simulation with ModelSim
testSimulationModelsim :: IO [(Bottle_Event, Return_Event)]
testSimulationModelsim = writeAndModelsimVHDL Nothing vendingMachineSys testSig

-- ==> Hardware Generation

-- IMPORTANT: Programming the DE10 Standard:
-- > quartus_pgm -c DE-SoC -m JTAG -o "p;./VendingMachine/vhdl/VendingMachine.sof@2"
generateHW_DE_10_Standard :: IO ()
generateHW_DE_10_Standard = writeVHDLOps vhdlOps vendingMachineSys
 where vhdlOps = defaultVHDLOps{execQuartus=Just quartusOps}
       quartusOps = QuartusOps{action=FullCompilation,
                               fMax=Just 50, -- in MHz
                               fpgaFamiliyDevice=Just ("Cyclone V",
                                                       Just "5CSXFC6D6F31C6"),
                               -- Possibility for Pin Assignments
                               pinAssigs=[("resetn", "PIN_AB30"),  -- SW0
                                          ("coin.isPresent", "PIN_Y27"),  -- SW1
                                          ("coin.value", "PIN_AB28"),  -- SW2  
                                          ("clock","PIN_AJ4"),    -- KEY[0]
                                          ("bottle.tup_1.isPresent", "PIN_AA24"),   -- LEDR[0]
                                          ("bottle.tup_1.value", "PIN_AB23"),   -- LEDR[1]
                                          ("bottle.tup_2.isPresent", "PIN_AC23"),   -- LEDR[2]
                                          ("bottle.tup_2.value", "PIN_AD24")   -- LEDR[3]
                                         ]
                              }
