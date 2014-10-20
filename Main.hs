{-# LANGUAGE OverloadedStrings, TupleSections, Arrows, ScopedTypeVariables #-}
module Main where

import Graphics.Blank hiding (Event)
import FRP.Yampa
import System.Random(mkStdGen)
import Data.Default

import FRP.Yampa.Canvas

import FRP.Yampa.Canvas.Utils 
import FRP.Yampa.Canvas.Waveform 
import FRP.Yampa.Canvas.Timeline
import FRP.Yampa.Canvas.Virtual

import FRP.Yampa.Protocol.RS232

import Text.Printf

import Data.Word (Word8)
import Data.Bits

main :: IO ()
main = blankCanvas 3000 $
       reactimateVirtualSFinContext 0.02 10 (\ _ -> NoEvent) program 

program :: SF (Event ()) (Canvas ())
program = proc _inp -> do
        t6 <- afterEach [(1 / fromIntegral baud * 16,i) | i <- [0..255]] -< ()
        r6 <- timelineSF def { timelineTime = timeslice, timelineRealEstate = (1000,100), timelineShow = \ _ a -> show a } -< t6

        t7 <- rs232_Tx baud -< t6
        t7' <- arr (\ x -> if x == Space then 1.0 else 0.0) -< t7
        r7 <- waveform def { waveformTime = timeslice, waveformRealEstate = (1000,100) } -< t7'

        t1 <- waitForSpace baud -< t7
        r8 <- timelineSF def { timelineTime = timeslice, timelineRealEstate = (1000,100) } -< t1

        t2 <- pulses baud -< t1
        r2 <- timelineSF def { timelineTime = timeslice, timelineRealEstate = (1000,100), timelineShow = \ v a -> printf "%.2f" v ++ " " ++ printf "%.2f" a } -< t2

        t3 <- rs232_rx baud -< t7
        r3 <- timelineSF def { timelineTime = timeslice, timelineRealEstate = (1000,100)
                             , timelineShow = \ _ vs -> show vs
                             } -< t3

        rs <- arr (\ cs -> sequence_ [ saveRestore $ do { translate (0,10 + n) ; c }
                                     | (n,c) <- [0,120..] `zip` cs
                                     ]) -< [r6,r7,r8,r2,r3]


	returnA -< rs
  where baud = 1
        timeslice = 100
-----------------------------------------------
