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
        w <- arr (sin . (/ 0.2)) <<< time -< ()
        r <- waveform def { waveformRange = (-1,1) } -< w
	wi <- integral -< w
        r' <- waveform def { waveformRange = (0,1) } -< wi
	t2 <- noise (mkStdGen 0) -< ()
        r2 <- waveform def { waveformRange = (0,1) } -< t2
        t3 <- (integral <<< arr (\ x -> x - 0.5)) -< t2
        r3 <- waveform def { waveformRange = (-1,1) } -< t3
        t4 <- spsSF -< ()
        r4 <- waveform def { waveformRange = (0,100) } -< t4

        -- sscan :: (b -> a -> b) -> b -> SF a b
        t5 <- repeatedly 1 () -< ()
        r5 <- timelineSF def -< t5

        t6 <- afterEach [(3,i) | i <- [0..255]] -< ()
        r6 <- timelineSF def { timelineShow = \ _ a -> show a } -< t6

        t7 <- rs232_Tx 4 -< t6
        t7' <- arr (\ x -> if x == Space then 1.0 else 0.0) -< t7
        r7 <- waveform def -< t7'

        rs <- arr (\ cs -> sequence_ [ saveRestore $ do { translate (0,10 + n) ; c }
                                     | (n,c) <- [0,120..] `zip` cs
                                     ]) -< [r,r',r2,r3,r4]

        rX <- arr (\ cs -> sequence_ [ saveRestore $ do { translate (550,10 + n) ; c }
                                     | (n,c) <- [0,120..] `zip` cs
                                     ]) -< [r5,r6,r7]


	returnA -< (rs >> rX)

-----------------------------------------------

data Line = Space | Mark deriving (Eq,Ord,Show)

-- This ignores events that are too close together.
-- In Yampa, make sure the baud is about a 5th of the (lower case) samples per second.
rs232_Tx :: Int -> SF (Event Word8) Line
rs232_Tx baud = switch waitForIt $ \ c -> 
        issue $ [Space] ++ [ if testBit c i
                             then Mark
                             else Space | i <- [0..7] ] ++ [Mark]
  where waitForIt = arr $ \ e -> (Mark,e)
--        holdWith :: Line -> SF a (Line, Event ())
        issue [] = rs232_Tx baud
        issue (c:cs) = switch (constant c &&& (after (1 / fromIntegral baud) ())) $ \ () ->
                       issue cs
                       

rs232_Tx' :: Int -> SF (Event Word8) (Event Ack,Line)
rs232_Tx' = undefined

rs232_rx :: Int -> SF Line (Event Word8)
rs232_rx baud = undefined


