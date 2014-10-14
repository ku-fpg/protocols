{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Main where

import Graphics.Blank hiding (Event)
import FRP.Yampa
import FRP.Yampa.Vector2
import Data.Text(Text,pack)
import Data.Monoid((<>))
import Control.Arrow

import FRP.Yampa.Canvas

main :: IO ()
main = blankCanvas 3000 $
       reactimateSFinContext (\ _ -> return NoEvent) id program

program :: SF (Event ()) (Canvas ())
program = time >>> (arr $ \ t -> do
 	font "20pt Comic Sans MS"
	fillText ("+ " <> pack (show t),50,50))

