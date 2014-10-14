module Main where

import Graphics.Blank hiding (Event)
import FRP.Yampa
import FRP.Yampa.Vector2
import Data.Text(Text)

import FRP.Yampa.Canvas

main :: IO ()
main = blankCanvas 3000 $
       reactimateSFinContext (\ _ -> return NoEvent) id program

program :: SF (Event ()) (Canvas ())
program = undefined


