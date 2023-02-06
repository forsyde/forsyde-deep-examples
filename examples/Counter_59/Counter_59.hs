{-# LANGUAGE TemplateHaskell #-}

module Counter_59 where

import ForSyDe.Deep
import Counter_5
import Counter_9
import SevenSegmentDecoder
import ForSyDe.Deep.Bit
import Data.Param.FSVec
import Data.Int
import Data.TypeLevel.Num.Reps
import Data.TypeLevel.Num.Aliases

type Run = Bit

systemProc :: Signal Run -> (Signal Bit, Signal (FSVec D7 Bit), Signal (FSVec D7 Bit))
systemProc run = (run_5, sevenSeg_5, sevenSeg_9)
   where
      sevenSeg_9 = (instantiate "sevenSegDec_9" sevenSegDecSys) counter_9_Out
      sevenSeg_5 = (instantiate "sevenSegDec_5" sevenSegDecSys) counter_5_Out
      (run_5, counter_5_Out) = (unzipSY "unzip_5" . instantiate "counter_5" counter_5_Sys) run_9
      (run_9, counter_9_Out) = (unzipSY "unzip_9" . instantiate "counter_9" counter_9_Sys) run
      

counter_59_Sys ::g SysDef (Signal Run -> (Signal Bit, Signal (FSVec D7 Bit), Signal (FSVec D7 Bit)))
counter_59_Sys = newSysDef systemProc "counter_59" ["run"] ["maxvalue", "sevenseg_5", "sevenseg_9"]


-- ==> Simulation with Modelsim
simulateModelsim :: [Bit] -> IO ([Bit], [FSVec D7 Bit], [FSVec D7 Bit])
simulateModelsim = writeAndModelsimVHDL Nothing counter_59_Sys

-- ==> Hardware Generation
-- IMPORTANT: Programming the DE10 Standard:
-- > quartus_pgm -c DE-SoC -m JTAG -o "p;./counter_59/vhdl/counter_59.sof@2"
generateHW_DE_10_Standard :: IO ()
generateHW_DE_10_Standard = writeVHDLOps vhdlOps counter_59_Sys
 where vhdlOps = defaultVHDLOps{execQuartus=Just quartusOps}
       quartusOps = QuartusOps{action=FullCompilation,
                               fMax=Just 50, -- in MHz
                               fpgaFamiliyDevice=Just ("Cyclone V",
                                                       Just "5CSXFC6D6F31C6"),
                               -- Possibility for Pin Assignments
                               pinAssigs=[("run", "PIN_AB30"),  -- SW0
                                          ("resetn", "PIN_Y27"),  -- SW1
                                          ("clock","PIN_AJ4"),    -- KEY[0]
                                          ("sevenseg_9[6]","PIN_W17"),   -- HEX0[0]
                                          ("sevenseg_9[5]","PIN_V18"),   -- HEX0[1]
                                          ("sevenseg_9[4]","PIN_AG17"),  -- HEX0[2]
                                          ("sevenseg_9[3]","PIN_AG16"),  -- HEX0[3]
                                          ("sevenseg_9[2]","PIN_AH17"),  -- HEX0[4]
                                          ("sevenseg_9[1]","PIN_AG18"),  -- HEX0[5]
                                          ("sevenseg_9[0]","PIN_AH18"),  -- HEX0[6]
                                          ("sevenseg_5[6]","PIN_AF16"), -- HEX1[0]
                                          ("sevenseg_5[5]","PIN_V16"), -- HEX1[1]
                                          ("sevenseg_5[4]","PIN_AE16"), -- HEX1[2]
                                          ("sevenseg_5[3]","PIN_AD17"), -- HEX1[3]
                                          ("sevenseg_5[2]","PIN_AE18"), -- HEX1[4]
                                          ("sevenseg_5[1]","PIN_AE17"),  -- HEX1[5]
                                          ("sevenseg_5[0]","PIN_V17"),   -- HEX1[6]
                                          ("maxvalue", "PIN_AA24") -- LEDR[0]
                                         ]
                              }

-- ==> Hardware Generation
-- IMPORTANT: Programming the DE2-35:
-- > nios2-configure-sof counter_59.sof"
generateHW_DE2_35 :: IO ()
generateHW_DE2_35= writeVHDLOps vhdlOps counter_59_Sys
 where vhdlOps = defaultVHDLOps{execQuartus=Just quartusOps}
       quartusOps = QuartusOps{action=FullCompilation,
                               fMax=Just 50, -- in MHz
                                fpgaFamiliyDevice=Just ("CycloneII",
                                                       Just "EP2C35F672C6"),
                               -- Possibility for Pin Assignments
                               pinAssigs=[("run", "PIN_N25"),  -- SW0
                                          ("resetn", "PIN_N26"),  -- SW1
                                          ("clock","PIN_G26"),    -- KEY[0]
                                          ("sevenseg_9[6]","PIN_AF10"),   -- HEX0[0]
                                          ("sevenseg_9[5]","PIN_AB12"),   -- HEX0[1]
                                          ("sevenseg_9[4]","PIN_AC12"),  -- HEX0[2]
                                          ("sevenseg_9[3]","PIN_AD11"),  -- HEX0[3]
                                          ("sevenseg_9[2]","PIN_AE11"),  -- HEX0[4]
                                          ("sevenseg_9[1]","PIN_V14"),  -- HEX0[5]
                                          ("sevenseg_9[0]","PIN_V13"),  -- HEX0[6]
                                          ("sevenseg_5[6]","PIN_V20"), -- HEX1[0]
                                          ("sevenseg_5[5]","PIN_V21"), -- HEX1[1]
                                          ("sevenseg_5[4]","PIN_W21"), -- HEX1[2]
                                          ("sevenseg_5[3]","PIN_Y22"), -- HEX1[3]
                                          ("sevenseg_5[2]","PIN_AA24"), -- HEX1[4]
                                          ("sevenseg_5[1]","PIN_AA23"),  -- HEX1[5]
                                          ("sevenseg_5[0]","PIN_AB24"),   -- HEX1[6]
                                          ("maxvalue", "PIN_AE23") -- LEDR[0]
                                         ]
                              }


