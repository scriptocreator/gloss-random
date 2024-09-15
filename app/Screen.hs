{-# LANGUAGE NamedFieldPuns #-}

module Screen (screen) where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Data.Picture
--import Graphics.UI.GLUT.Fonts as GLUTF
--import Graphics.UI.GLFW as GLFW
import System.Random
import System.Random.Stateful
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)


initialState :: (Int,Int) -> (Int,Int) -> Figure
initialState (rpx,rpy) (rsx,rsy) =
  Figure
    (toEnum rpx, toEnum rpy)
    0
    (toEnum rpx, toEnum rsy)
    Круг
    (dark red)
    []



render :: Figure -> Picture
render (figure@(Figure {fgP, fgT, fgC})) =
  pictures [ translate (fst fgP) (snd fgP) $ color fgC $ tfigure fgT
           , translate 0 0 $ color fgC $ text "Boo!"]
           --, translate 0 0 $ color ds $ rectangleSolid (-fromInteger (snd disp)/2) (fromInteger (fst disp)/2)]

  where tfigure Прямоугольник = rectangleSolid 10 10
        tfigure Круг = circleSolid 10



handleKeys :: Event -> Figure -> Figure
{-
handleKeys (EventKey (Char 'ф') Up a b) figure =
  handleKeys (EventKey (Char 'a') Up a b) figure

handleKeys (EventKey (Char 'в') Up a b) figure =
  handleKeys (EventKey (Char 'd') Up a b) figure

handleKeys (EventKey (Char 'ц') Up a b) figure =
  handleKeys (EventKey (Char 'w') Up a b) figure
-}
handleKeys (EventKey (Char 'a') Up _ _) figure =
  figure
  `spd` (fgS figure *% ((-),50))

handleKeys (EventKey (Char 'd') Up _ _) figure =
  figure
  `spd` (fgS figure *% ((+),50))

handleKeys (EventKey (Char 'w') Up _ _) figure =
  figure
  `spd` (fgS figure %* ((+),50))

handleKeys (EventKey (Char 's') Up _ _) figure =
  figure
  `spd` (fgS figure %* ((-),50))

handleKeys (EventKey (Char '/') Up _ _) (figure@(Figure {fgT=Прямоугольник})) =
  figure
  `typ` Круг

handleKeys (EventKey (Char '/') Up _ _) (figure@(Figure {fgT=Круг})) =
  figure
  `typ` Прямоугольник

handleKeys (EventKey (Char 'r') Up _ _) figure =
  figure
  `col` blue

handleKeys (EventKey (Char 'g') Up _ _) figure =
  figure
  `col` green

handleKeys (EventKey (Char 'b') Up _ _) figure =
  figure
  `col` blue

handleKeys (EventKey (Char '-') Up _ _) figure =
  figure
  `col` (dark $ fgC figure)

handleKeys (EventKey (Char '=') Up _ _) figure =
  figure
  `col` (light $ fgC figure)

handleKeys _ game = game



update :: (Int,Int) -> Float -> Figure -> Figure
update g s = collision g . position s

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



screen :: (Int,Int) -> IO ()
screen g = do
  rp <- randomRIO ( (div (-fst g) 2, div (-snd g) 2), (div (fst g) 2, div (snd g) 2) )
  rs <- randomRIO ( (div (-400) 2, div (-400) 2), (div 400 2, div 400 2) )

  print g
  print rp
  print rs

  play
   window background fps
   (initialState rp rs)
   render handleKeys (update g)
