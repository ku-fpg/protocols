{-# LANGUAGE OverloadedStrings, TupleSections, Arrows, RankNTypes, ScopedTypeVariables #-}
module FRP.Yampa.Canvas.Virtual (reactimateVirtualSFinContext) where

import FRP.Yampa

import Data.Time.Clock
import Data.IORef
import Control.Concurrent.STM

import Graphics.Blank hiding (Event)
import qualified Graphics.Blank as Blank

-------------------------------------------------------------------

-- | Redraw the entire canvas.
renderCanvas ::  DeviceContext -> Canvas () -> IO ()
renderCanvas context drawAction = send context canvas
  where
    canvas :: Canvas ()
    canvas = do clearCanvas
                beginPath ()
                saveRestore drawAction

-------------------------------------------------------------------

-- | A specialisation of 'FRP.Yampa.reactimate' to Blank Canvas.
--   The arguments are: the Canvas action to get input, the Canvas action to emit output, the signal function to be run, and the device context to use.
reactimateVirtualSFinContext
      :: forall a .
        Double                 -- time interval
     -> Int                    -- How often to update the screen
     -> (Blank.Event -> Event a)
     -> SF (Event a) (Canvas ())  -- only update on *events*
     -> DeviceContext -> IO ()
reactimateVirtualSFinContext gap count interpEvent sf context =
  do c <- newIORef (cycle [1..count])
     let getInput :: Bool -> IO (DTime,Maybe (Event a))
         getInput canBlock =
            do let opt_block m =
                            if canBlock
                            then m
                            else m `orElse` return Nothing

               opt_e <- atomically $ opt_block $ fmap Just $ readTChan (eventQueue context)
               ev <- case opt_e of
                       Nothing -> return NoEvent
                       Just e  -> return (interpEvent e)

               return (gap, Just ev)

         putOutput :: Bool -> Canvas () -> IO Bool
         putOutput changed b = do
                 (i:is) <- readIORef c
                 writeIORef c is
                 if changed && i == 1
                 then renderCanvas context b >> return False
                 else return False

     reactimate (return NoEvent) getInput putOutput sf

-------------------------------------------------------------------

