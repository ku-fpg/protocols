{-# LANGUAGE OverloadedStrings, TupleSections, Arrows #-}
module Main where

import Graphics.Blank hiding (Event)
import FRP.Yampa
import System.Random(mkStdGen)
import Data.Default

import FRP.Yampa.Canvas

import FRP.Yampa.Canvas.Waveform 
import FRP.Yampa.Canvas.Utils 


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
        t4 <- arr fromIntegral  <<< spsSF -< ()
        r4 <- waveform def { waveformRange = (0,100) } -< t4

        -- sscan :: (b -> a -> b) -> b -> SF a b
        t5 <- arr (\ x -> if isEvent x then 1 else 0) <<< repeatedly 1 () -< ()
        r5 <- waveform def { waveformRange = (0,1) } -< t5

        rs <- arr (\ cs -> sequence_ [ saveRestore $ do { translate (0,10 + n) ; c }
                                     | (n,c) <- [0,120..] `zip` cs
                                     ]) -< [r,r',r2,r3,r4]

        rX <- arr (\ cs -> sequence_ [ saveRestore $ do { translate (550,10 + n) ; c }
                                     | (n,c) <- [0,120..] `zip` cs
                                     ]) -< [r5]


	returnA -< (rs >> rX)

-----------------------------------------------

-- An event timeline.
timeline :: Waveform -> SF (Event String) (Canvas ())
timeline = undefined

-- This should move to the bridge library
record :: Double                    -- ^ frames per second
       -> Time                      -- ^ end of time (start is always 0)
       -> [(Time,a)]                -- ^ event values, with timestamp
       -> FilePath                  -- ^ directory to put the png files into
       -> SF (Event a) (Canvas ())  -- Signal Function 
       -> IO ()
record = undefined
