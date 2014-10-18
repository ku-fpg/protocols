{-# LANGUAGE OverloadedStrings, TupleSections, Arrows #-}
module FRP.Yampa.Canvas.Timeline 
        ( Timeline(..)
        , timelinePaint
        , timelineSF
        ) where

import Graphics.Blank hiding (Event)
import FRP.Yampa
import FRP.Yampa.Vector2
import Data.Text(Text,pack)
import Data.Monoid((<>))
import Control.Arrow
import System.Random(mkStdGen)
import Text.Printf
import Data.Default

import FRP.Yampa.Canvas

import FRP.Yampa.Canvas.Utils


data Timeline a = Timeline
        { timelineRealEstate  :: (Double,Double)        -- ^ size on screen
        , timelineTime        :: Double                 -- ^ length of time to record
        , timelineFont        :: Text                   -- ^ sans-serif
        , timelineAbove       :: Time -> a -> String    -- ^ above the dot, default time
        , timelineBelow       :: Time -> a -> String    -- ^ below the dot, default empty
        }

instance Default (Timeline a) where
  def = Timeline (500,100) 10 "10pt sans-serif" (\ a _ -> printf "%.2f" a) (\ _ a -> "")

-- The pure painter
timelinePaint :: Timeline a -> [(Time,Event a)] -> Canvas ()
timelinePaint tl ts | length ts < 1 = return ()
timelinePaint tl ts = do
        strokeStyle "blue"
        -- find y-axis
        let y_axis = h / 2
        beginPath()
        -- (mn,mx)
        moveTo (0,y_axis)
        lineTo (w,y_axis)
        lineWidth 1
        stroke()
        sequence_ [ do
               beginPath()
               let x = (1 + (t - mx) / timelineTime tl) * w
               arc (x,y_axis,3,0,2*pi,False) 
               fillStyle "blue"
               fill() 
            | (t,e) <- ts1 ]

  where ts1 = [ (t,e) | (t,Just e) <- [ (t,event Nothing Just ev) | (t,ev) <- ts ] ]
        mx = maximum $ map fst ts
        (w,h) = timelineRealEstate tl

-- The signal function version. The only SF part is the scopeSF
timelineSF :: Timeline a -> SF (Event a) (Canvas ())
timelineSF tl = scopeSF (timelineTime tl) >>> arr (timelinePaint tl)

