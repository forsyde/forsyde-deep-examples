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
      

system :: SysDef (Signal Run -> (Signal Bit, Signal (FSVec D7 Bit), Signal (FSVec D7 Bit)))
system = newSysDef systemProc "system" ["run"] ["run_5", "sevenseg_5", "sevenseg_9"]


-- Hardware Generation
compileQuartus_CounterSystem :: IO ()
compileQuartus_CounterSystem = writeVHDLOps vhdlOps system
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
                                          ("run_5", "PIN_AA24") -- LEDR[0]
                                         ]
                              }
