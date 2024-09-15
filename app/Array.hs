{-# LANGUAGE NamedFieldPuns #-}

module Array (array) where

import Lib
import Prelude
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
--import Graphics.UI.GLUT.Fonts as GLUTF
--import Graphics.UI.GLFW as GLFW
import System.Random
import System.Random.Stateful
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List (zip4)


initialState :: [((Int,Int), (Int,Int), Type, Color)] -> Figures
initialState =
  fmap
  (\((px,py),(sx,sy),t,c) ->
    Figure
      (toEnum px,toEnum py)
      0
      (toEnum sx,toEnum sy)
      t
      c
      [])



render :: Figures -> Picture
render lf =
  pictures $ fmap
    (\(Figure {fgP,fgT,fgC}) ->
       translate (fst fgP) (snd fgP) $ color fgC $ tfigure fgT)
    lf

  where tfigure Прямоугольник = rectangleSolid 10 10
        tfigure Круг = circleSolid 10



handleKeys :: Event -> Figures -> Figures
handleKeys _ game = game



update :: (Int,Int) -> Float -> Figures -> Figures
update g s =
  fmap
  (collision g . position s)

position :: Float -> Figure -> Figure
position s (figure@(Figure {fgP,fgS})) =
  figure
  `pos` (px', py')

  where px' = fst fgP + fst fgS * s
        py' = snd fgP + snd fgS * s

collision :: (Int,Int) -> Figure -> Figure
collision (dx,dy) (figure@(Figure {fgP,fgS})) =
  figure
  `spd` (sxc', syc')

  where sc' p d s =
          if state p d
          then -s
          else s
        sxc' = sc' (fst fgP) dx (fst fgS)
        syc' = sc' (snd fgP) dy (snd fgS)



array :: (Int,Int) -> Int -> IO ()
array g numer = do

  rFPosits <- replicateM numer (randomRIO ( (div (-fst g) 2, div (-snd g) 2), (div (fst g) 2, div (snd g) 2) ))
  rFSpeeds <- replicateM numer (randomRIO ( (div (-400) 2, div (-400) 2), (div 400 2, div 400 2) ))
  rFbools <- replicateM numer (randomIO :: IO Bool)
  rFthrees <- replicateM numer (randomRIO ((1,3) :: (Int,Int)))

  let zipper =
        zip4
          rFPosits
          rFSpeeds
          rFTypes
          rFColors

      rFTypes = rt rFbools
      rFColors = rc rFthrees

  play
   window background fps (initialState zipper) render handleKeys (update g)

  where rt (True:xs) = Круг:rt xs
        rt (False:xs) = Прямоугольник:rt xs
        rt [] = []

        rc (1:xs) = red:rc xs
        rc (2:xs) = green:rc xs
        rc (3:xs) = blue:rc xs
        rc (_:xs) = rc xs
        rc [] = []
