{-# LANGUAGE OverloadedStrings, TupleSections, Arrows #-}
module Main where

import Graphics.Blank hiding (Event)
import FRP.Yampa
import System.Random(mkStdGen)
import Data.Default

import FRP.Yampa.Canvas

import FRP.Yampa.Canvas.Utils 
import FRP.Yampa.Canvas.Waveform 
import FRP.Yampa.Canvas.Timeline

import Data.Word (Word8)
import Data.Bits

main :: IO ()
main = blankCanvas 3000 $
       reactimateSFinContext (\ _ -> return NoEvent) id program

--	  sscan (\ :: (b -> a -> b) -> b -> SF a b

program :: SF (Event ()) (Canvas ())
program = proc _inp -> do
        t6 <- afterEach [(3,i) | i <- [0..255]] -< ()
        r6 <- timelineSF def { timelineTime = 20, timelineRealEstate = (1000,100), timelineShow = \ _ a -> show a } -< t6

        t7 <- rs232_Tx 4 -< t6
        t7' <- arr (\ x -> if x == Space then 1.0 else 0.0) -< t7
        r7 <- waveform def { waveformTime = 20, waveformRealEstate = (1000,100) } -< t7'

        t1 <- waitForSpace 4 -< t7
        r8 <- timelineSF def { timelineTime = 20, timelineRealEstate = (1000,100) } -< t1

        rs <- arr (\ cs -> sequence_ [ saveRestore $ do { translate (0,10 + n) ; c }
                                     | (n,c) <- [0,120..] `zip` cs
                                     ]) -< [r6,r7,r8]


	returnA -< rs

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

--rs232_rx :: Int -> SF Line (Event ()) -- (Event Word8)
--rs232_rx baud = waitForSpace baud `switch` \ () ->
        

-- This says nothing about frequency of sampling, which
-- is (from FPGA hacking) typically 16x the baud rate.
waitForSpace :: Int -> SF Line (Event Time)
waitForSpace baud = (arr f >>> edge >>> delayEvent (1 / (2 * fromIntegral baud))) >>> timeEvents
        
  where f Space = True
        f Mark   = False

-- The first event is given time 0.
timeEvents :: SF (Event a) (Event Time)
timeEvents = (never &&& identity) `switch` \ _ -> 
        proc inp -> do 
                r <- now ()                                     -< ()
                t <- time                                       -< ()
                arr (\ (t,r) -> tagWith t r) -< (t,r)

-- This is a type of bind!
--bind :: SF a (Event b) -> (b -> SF a (Event b)) -> SF a (Event b)
--s `bind` k = s `switch` k

