{-# LANGUAGE OverloadedStrings, TupleSections, Arrows #-}
module FRP.Yampa.Canvas.Waveform 
        ( Waveform(..)
        , waveformPaint
        , waveform
        ) where

import Graphics.Blank hiding (Event)
import FRP.Yampa
import Data.Text(Text,pack)
import Text.Printf
import Data.Default

data Waveform = Waveform 
        { waveformRealEstate  :: (Double,Double) -- ^ size on screen
        , waveformTime        :: Double          -- ^ length of time to record
        , waveformRange       :: (Double,Double) -- ^ min and max values
        , waveformFont        :: Text            -- ^ sans-serif
        , waveformString      :: String          -- ^ formatting for y-axis label
        , waveformInterpolate :: Bool            -- ^ do we draw lines between the points?
        }

instance Default Waveform where
  def = Waveform (500,100) 10 (0,1) "10pt sans-serif" "%.2f" True

-- The pure painter
waveformPaint :: Waveform -> [(Time,Double)] -> Canvas ()
waveformPaint = undefined

-- Record the significant changes.
--shrink :: Double -> (Time,Double) -> [(Time,Double)] -> [(Time,Double)]
--shrink

mesh :: Eq b => [(a, b)] -> [(a, b)]
mesh ((a1,b1):(_,b2):(a3,b3):abs') | b1 == b2 && b2 == b3 = mesh ((a1,b1):(a3,b3):abs')
mesh (ab:abs') = ab : mesh abs'
mesh [] = []

-- The signal function version
waveform :: Waveform -> SF Double (Canvas ())
waveform (Waveform (w,h) ws (mn,mx) theFont st interp) = proc inp -> do
         t    <- time -< ()
         inp' <- arr norm'' -< inp
         st'  <- sscan (\ vs (tm,v) -> (tm / ws,v) : mesh [ v' | v'@(tm',_) <- vs,  tm'  >= tm / ws - 1 ]) [] -< (t,inp')
         st''  <- arr norm' -< st'
         c1   <- arr (fn' (w,h)) -< st''
         returnA -< c1
  where
        -- The values are in the range (0,1) in both the x and y directions.
        fn' _ vs | length vs < 2 =  return ()
        fn' (ws',hs) xys = saveRestore $ do
                strokeStyle "blue"
                -- find y-axis
                let y_axis = (1 - mn / (mn - mx)) * h
                beginPath()
                -- (mn,mx)
                moveTo (0,y_axis)
                lineTo (w,y_axis)
                lineWidth 0.5
                stroke()
                beginPath()
                moveTo (w,0)
                lineTo (w,h)
                lineWidth 0.5
                stroke()
                if interp
                then do beginPath()
                        moveTo $ head $ xys'
                        mapM_ lineTo $ tail $ xys'
                        lineWidth 1
                        strokeStyle "blue"
                        stroke()
                else sequence_ $ [ do 
                        beginPath()
                        arc (fst xy,snd xy,1,0,2*pi,False) 
                        fillStyle "blue"
                        fill() | xy <- tail $ xys' ]
                beginPath()
                arc(fst $ head $ xys', snd $ head $ xys', 2, 0, 2 * pi, False)
                fillStyle "blue"
                fill()
                font theFont
                textBaseline "middle"
                fillText (pack $ printf st mx,w + 5,0)
                fillText (pack $ printf st mn,w + 5,h)
                fillText (pack $ printf st $ ((+) mn) $ (* (mx - mn)) $ snd $ head $ xys,w + 5,snd $ head $ xys')
                fillText (pack $ show (length xys),20,20)

          where
                xys' = [ (x * ws', (1 - y) * hs) | (x,y) <- xys ]



        norm' :: [ (Double,Double)] -> [(Double,Double)]
        norm' vs 
          | null vs = []  -- need at least one point
          | otherwise = [ ((x' + delta_x), y') | (x',y') <- vs ]
          where max_x = maximum $ map fst $ vs
                delta_x = 1 - max_x

        norm'' d = (max mn (min mx d) - mn) / (mx - mn)
        
-- waveform' :: (Double,Double) -> (Double,Double) -> SF Double (Canvas ())
-- waveform' (w,h) (ws,hs) = proc inp -> do
--          t <- time -< ()
--          st <- sscan (\ vs (tm,v) -> (tm / ws,v) : [ v | v@(tm',_) <- vs,  tm'  >= tm / ws - 1 ]) [] -< (t,inp)
--          st' <- arr norm -< st
--          st2 <- arr (\ _ -> [(1.0,1.0),(0.99,0.0),(0,0)]) -< ()
--          c1 <- arr (fn (w,h)) -< st'
--          returnA -< c1
--   where
--         norm :: [ (Double,Double)] -> [(Double,Double)]
--         norm vs 
--           | null vs = []  -- need at least one point
--           | otherwise = [ ((x' + delta_x), y') | (x',y') <- vs ]
--           where max_x = maximum $ map fst $ vs
--                 delta_x = 1 - max_x

-- The values are in the range (0,1) in both the x and y directions.
-- fn _ vs | length vs < 2 =  return ()
-- fn (ws,hs) xys = saveRestore $ do
--         beginPath()
--         moveTo $ head $ xys'
--         mapM_ lineTo $ tail $ xys'
--         lineWidth 1
--         strokeStyle "blue"
--         stroke()
--         beginPath()
--         arc(fst $ head $ xys', snd $ head $ xys', 3, 0, 2 * pi, False)
--         fillStyle "blue"
--         fill()
--   where
--         xys' = [ (x * ws, (1 - y) * hs) | (x,y) <- xys ]

