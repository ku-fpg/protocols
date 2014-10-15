{-# LANGUAGE OverloadedStrings, TupleSections, Arrows #-}
module Main where

import Graphics.Blank hiding (Event)
import FRP.Yampa
import FRP.Yampa.Vector2
import Data.Text(Text,pack)
import Data.Monoid((<>))
import Control.Arrow
import System.Random(mkStdGen)

import FRP.Yampa.Canvas

main :: IO ()
main = blankCanvas 3000 $
       reactimateSFinContext (\ _ -> return NoEvent) id program

--	  sscan (\ :: (b -> a -> b) -> b -> SF a b

program :: SF (Event ()) (Canvas ())
program = proc _inp -> do
        w <- arr (sin . (/ 0.2)) <<< time -< ()
        r <- waveform defaultWaveform { waveformRange = (-1,1) } -< w
	wi <- integral -< w
        r' <- waveform defaultWaveform { waveformRange = (-1,1) } -< wi
	t2 <- noise (mkStdGen 0) -< ()
        r2 <- waveform' (500,100) (10,1) -< t2
        t3 <- (integral <<< arr (\ x -> x - 0.5)) -< t2
        r3 <- waveform' (500,100) (10,1) -< t3
        t4 <- arr (\x -> fromIntegral x/100) <<< sps -< ()
        r4 <- waveform' (500,100) (10,1) -< t4
        rs <- arr (\ cs -> sequence_ [ saveRestore $ do { translate (0,n) ; c }
                                     | (n,c) <- [0,100..] `zip` cs
                                     ]) -< [r,r',r2,r3,r4]
	returnA -< rs

-- how may samples per second?
sps :: SF () Int
sps = time >>> sscan (\ vs tm -> tm : [ v | v <- vs, v >= tm - 1]) [] >>> arr length

data Waveform = Waveform 
        { waveformRealEstate :: (Double,Double) -- size on screen
        , waveformTime       :: Double          -- length of time to record
        , waveformRange      :: (Double,Double) -- min and max values
        }

defaultWaveform = Waveform (500,100) 10 (0,1)

waveform :: Waveform -> SF Double (Canvas ())
waveform (Waveform (w,h) ws (mn,mx)) = arr norm >>> waveform' (w,h) (ws,1)
  where norm d = (max mn (min mx d) - mn) / (mx - mn)
        
waveform' :: (Double,Double) -> (Double,Double) -> SF Double (Canvas ())
waveform' (w,h) (ws,hs) = proc inp -> do
	 t <- time -< ()
	 st <- sscan (\ vs (tm,v) -> (tm / ws,v) : [ v | v@(tm',_) <- vs,  tm'  >= tm / ws - 1 ]) [] -< (t,inp)
         st' <- arr norm -< st
         st2 <- arr (\ _ -> [(1.0,1.0),(0.99,0.0),(0,0)]) -< ()
         c1 <- arr (fn (w,h)) -< st'
         c2 <- showCanvasSF -< st
	 returnA -< c1
  where
        norm :: [ (Double,Double)] -> [(Double,Double)]
        norm vs 
          | null vs = []  -- need at least one point
          | otherwise = [ ((x' + delta_x), y') | (x',y') <- vs ]
          where max_x = maximum $ map fst $ vs
                delta_x = 1 - max_x

-- The values are in the range (0,1) in both the x and y directions.
fn _ vs | length vs < 2 =  return ()
fn (ws,hs) xys = saveRestore $ do
        beginPath()
        moveTo $ head $ xys'
        mapM_ lineTo $ tail $ xys'
        lineWidth 1
        strokeStyle "blue"
        stroke()
        beginPath()
        arc(fst $ head $ xys', snd $ head $ xys', 5, 0, 2 * pi, False)
        fillStyle "blue"
        stroke()
  where
        xys' = [ (x * ws, (1 - y) * hs) | (x,y) <- xys ]


-----------------------------------------------

-- An event timeline.
timeline :: Waveform -> SF (Event String) (Canvas ())
timeline = undefined

showCanvasSF :: (Show a) => SF a (Canvas ())
showCanvasSF = arr $ \ a -> saveRestore $ do
        font "8pt Comic Sans MS"
        fillText (pack (show a),50,50)
