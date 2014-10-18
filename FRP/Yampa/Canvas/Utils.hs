{-# LANGUAGE OverloadedStrings, TupleSections, Arrows #-}
module FRP.Yampa.Canvas.Utils
        ( scopeSF
        , spsSF
        ) where

import FRP.Yampa


-- | Capture a specific number of seconds of recent history.
scopeSF :: Double -> SF a [(Time,a)]
scopeSF d = proc inp -> do
         t <- time -< ()
	 sscan (\ vs (tm,v) -> (tm,v) : [ v' | v'@(tm',_) <- vs,  tm'  >= tm - d ]) [] -< (t,inp)

-- | How may samples per second?
spsSF :: SF () Double
spsSF = time >>> sscan (\ vs tm -> tm : [ v | v <- vs, v >= tm - 1]) [] >>> arr len
  where len []  = 0
        len [x] = 1
        len xs  = fromIntegral (length xs) / (maximum xs - minimum xs) 


-- This should move to the bridge library
record :: Double                    -- ^ frames per second
       -> Time                      -- ^ end of time (start is always 0)
       -> [(Time,a)]                -- ^ event values, with timestamp
       -> FilePath                  -- ^ directory to put the png files into
       -> SF (Event a) (Canvas ())  -- Signal Function 
       -> IO ()
record = undefined
