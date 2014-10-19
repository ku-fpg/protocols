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

import Text.Printf

import Data.Word (Word8)
import Data.Bits

main :: IO ()
main = blankCanvas 3000 $
       reactimateVirtualSFinContext 0.01 10 (\ _ -> NoEvent) program 

program :: SF (Event ()) (Canvas ())
program = proc _inp -> do
        t6 <- afterEach [(1 / fromIntegral baud * 16,i) | i <- [0..255]] -< ()
        r6 <- timelineSF def { timelineTime = 20, timelineRealEstate = (1000,100), timelineShow = \ _ a -> show a } -< t6

        t7 <- rs232_Tx baud -< t6
        t7' <- arr (\ x -> if x == Space then 1.0 else 0.0) -< t7
        r7 <- waveform def { waveformTime = 20, waveformRealEstate = (1000,100) } -< t7'

        t1 <- waitForSpace baud -< t7
        r8 <- timelineSF def { timelineTime = 20, timelineRealEstate = (1000,100) } -< t1

        t2 <- pulses baud -< t1
        r2 <- timelineSF def { timelineTime = 20, timelineRealEstate = (1000,100), timelineShow = \ v a -> printf "%.2f" v ++ " " ++ printf "%.2f" a } -< t2

        t3 <- rs232_rx baud -< t7
        r3 <- timelineSF def { timelineTime = 20, timelineRealEstate = (1000,100)
                             , timelineShow = \ _ vs -> show vs
                             } -< t3

        rs <- arr (\ cs -> sequence_ [ saveRestore $ do { translate (0,10 + n) ; c }
                                     | (n,c) <- [0,120..] `zip` cs
                                     ]) -< [r6,r7,r8,r2,r3]


	returnA -< rs
  where baud = 1
-----------------------------------------------

data Line = Space | Mark deriving (Eq,Ord,Show)

-- This ignores events that are too close together.
-- In Yampa, make sure the baud is about a 5th of the (lower case) samples per second.
-- I use baud of 4, because I'm getting 20~30 updates a second.

rs232_Tx :: Int -> SF (Event Word8) Line
rs232_Tx baud = switch (waitForWith Mark) $ \ c -> 
        issue $ [Space] ++ [ if testBit c i
                             then Mark
                             else Space | i <- [0..7] ] ++ [Mark]
  where waitForWith :: c -> SF (Event e) (c,Event e)
        waitForWith x = arr $ \ e -> (x,e)
        baudPause :: SF a (Event ())
        baudPause = after (1 / fromIntegral baud) ()
        issue [] = rs232_Tx baud
        issue (c:cs) = switch (baudPause >>> waitForWith c) $ \ () ->
                       issue cs
                       
data Ack = Ack

rs232_Tx' :: Int -> SF (Event Word8) (Event Ack,Line)
rs232_Tx' = undefined

rs232_rx :: Int -> SF Line (Event Rx232)
rs232_rx baud = (rs232_single_rx baud >>> arr (\ a -> (a,a))) `dSwitch` (\ t  -> rs232_rx baud)
-- nt --> rs232_rx baud)

rs232_single_rx :: Int -> SF Line (Event Rx232)
rs232_single_rx baud = proc inp -> do
        trigger <- pulses baud <<< waitForSpace baud            -< inp
        samples <- arr (\ (t,i) -> attach t i)                  -< (trigger,inp)
        results <- accumBy f (Progress 0 0)                     -< samples 
        dropEvents 9 -< results
--        (onEvent $ \ c -> now c)  -< results
  where f (Progress i w) (t,ln) = case (i,round t,ln) of
                 (0,0,Space) -> Progress 1 0
                 (0,_,_)     -> Failed $ show ("failed start bit",i,printf "%.2f" t::String,ln)
                 (9,9,Mark)  -> Done w
                 (9,_,_)     -> Failed $ show ("failed stop bit",i,printf "%.2f" t::String,ln)
                 (i,j,x)| i == j 
                             -> Progress (i+1) (if x == Mark then setBit w (i-1) else w)
                 _           -> Failed "general failure"
        f (Done _)       _  = Failed "too many events" -- should be no more events after finishing
        f (Failed m)     _  = Failed m
  
data Rx232 = Progress Int Word8
           | Done Word8
           | Failed String
  deriving Show

-- This says nothing about frequency of sampling, which
-- is (from FPGA hacking) typically 16x the baud rate.
waitForSpace :: Int -> SF Line (Event Time)
waitForSpace baud = arr f >>> edge >>> timeEvents
  where f Space = True
        f Mark   = False

-- The first event turned into a time event.
timeEvents :: SF (Event a) (Event Time)
timeEvents = (never &&& identity) `switch` \ _ -> 
        proc inp -> do 
                r <- now ()                                     -< ()
                t <- time                                       -< ()
                arr (\ (t,r) -> tagWith t r) -< (t,r)

-- This is a type of bind!
--bind :: SF a (Event b) -> (b -> SF a (Event b)) -> SF a (Event b)
--s `bind` k = s `switch` k

onEvent :: (c -> SF (Event c) (Event b)) -> SF (Event c) (Event b)
onEvent k = (never &&& identity) `switch` k


-- Take a single (global) time event, and turn it into a stream of bit-counts, with spacing.
pulses :: Int -> SF (Event Time) (Event Double)
pulses baud = onEvent $ \ t0 -> proc _ -> do
         p <- afterEach [ (w / fromIntegral baud,())
                        | w <- 0.5 : repeat 1.01
                        ]                                            -< ()
         t <- time                                                   -< ()
         r <- arr (\ (p',t') -> fmap (\ () -> fromIntegral baud * (t' - t0) - 0.5) p')                -< (p,t)
         takeEvents 10 -< r
         
