{-# LANGUAGE NamedFieldPuns #-}

module Ball (ball) where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.Random.Stateful
import Control.Monad


initialState :: Figure
initialState =
  Figure
    (0,0)
    0
    (50,50)
    Круг
    (dark red)
    []



render :: Figure -> Picture
render (figure@(Figure {fgP, fgT, fgC})) =
  pictures [ translate (fst fgP) (snd fgP) $ color fgC $ tfigure fgT]
           --, translate 0 0 $ color ds $ rectangleSolid (-fromInteger (snd disp)/2) (fromInteger (fst disp)/2)]
   where
    tfigure Прямоугольник = rectangleSolid 10 10
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
handleKeys (EventKey (Char 'a') Up _ _) (figure@(Figure {fgS})) =
  figure
  `spd` (fgS *% ((-),50))

handleKeys (EventKey (Char 'd') Up _ _) (figure@(Figure {fgS})) =
  figure
  `spd` (fgS *% ((+),50))

handleKeys (EventKey (Char 'w') Up _ _) (figure@(Figure {fgS})) =
  figure
  `spd` (fgS %* ((+),50))

handleKeys (EventKey (Char 's') Up _ _) (figure@(Figure {fgS})) =
  figure
  `spd` (fgS %* ((-),50))

handleKeys (EventKey (Char '/') Up _ _) (figure@(Figure {fgT=Прямоугольник})) =
  figure
  `typ` Круг

handleKeys (EventKey (Char '/') Up _ _) (figure@(Figure {fgT=Круг})) =
  figure
  `typ` Прямоугольник

handleKeys (EventKey (Char '0') Up _ _) figure =
  figure
  `pos` (0, 0)
  `spd` (50,50)

handleKeys _ game = game



update :: (Int,Int) -> Float -> Figure -> Figure
update d s = collision d . position s

position :: Float -> Figure -> Figure
position s (figure@(Figure {fgP,fgS})) =
  figure
  `pos` (px', py')

  where px' = fst fgP + fst fgS * s
        py' = snd fgP + snd fgS * s

collision :: (Int,Int) -> Figure -> Figure
collision (dx,dy) (figure@(Figure {fgP,fgS})) =
  figure
  `spd` (fst sxc', fst syc')
  `col` mixc'

  where sc' p d s =
          if state p d
          then (-s, dark blue)
          else (s, dark red)

        sxc' = sc' (fst fgP) dx (fst fgS)
        syc' = sc' (snd fgP) dy (snd fgS)
        mixc' = mixColors 50 50 (snd sxc') (snd syc')



ball :: (Int,Int) -> IO ()
ball d = play window background fps initialState render handleKeys (update d)
