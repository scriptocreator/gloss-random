module Main (main) where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.Random.Stateful
import Control.Monad


type Position = (Float, Float)
type Speed = (Float, Float)
data Type = Прямоугольник | Круг
type Figures = Figure

data Figure = Figure Position Speed Type Color


scene = mixColors 50 50 green orange
disp :: Num a => (a, a)
disp = (900, 1600)
dr = dark red
ds = dark scene

window :: Display
window = FullScreen

background :: Color
background = scene

fps :: Int
fps = 60

initialState :: Figure
initialState = Figure (0,0) (50,50) Круг (dark red)


render :: Figure -> Picture
render (Figure (px,py) _ ff cdr) =
  pictures [ translate px py $ color cdr $ figure ff]
           --, translate 0 0 $ color ds $ rectangleSolid (-fromInteger (snd disp)/2) (fromInteger (fst disp)/2)]
   where
    figure Прямоугольник = rectangleSolid 10 10
    figure Круг = circleSolid 10


handleKeys :: Event -> Figure -> Figure
handleKeys (EventKey (Char 'a') _ _ _) (Figure a (sx,sy) ff c) = Figure a (sx-50,sy) ff c
handleKeys (EventKey (Char 'd') _ _ _) (Figure a (sx,sy) ff c) = Figure a (sx+50,sy) ff c
handleKeys (EventKey (Char 'w') _ _ _) (Figure a (sx,sy) ff c) = Figure a (sx,sy+50) ff c
handleKeys (EventKey (Char 's') _ _ _) (Figure a (sx,sy) ff c) = Figure a (sx,sy-50) ff c
handleKeys (EventKey (Char '/') Up _ _) (Figure a sn Прямоугольник c) = Figure a sn Круг c
handleKeys (EventKey (Char '/') Up _ _) (Figure a sn Круг c) = Figure a sn Прямоугольник c
handleKeys (EventKey (Char '0') _ _ _) (Figure (_,_) _ ff c) = Figure (0, 0) (50,50) ff c
handleKeys _ game = game


update :: Float -> Figure -> Figure
update s = collision . position s

position :: Float -> Figure -> Figure
position s (Figure (px,py) (sx,sy) ff c) = Figure (px', py') (sx,sy) ff c
  where
   px' = px + sx * s
   py' = py + sy * s

collision :: Figure -> Figure
collision (Figure (px,py) (sx,sy) ff c) = Figure (px,py) (fst sxc', fst syc') ff mixc'
  where
   mixc' = mixColors 50 50 (snd sxc') (snd syc')
   sxc' = if state px (snd disp) then ((-sx), dark blue) else (sx, dark red)
   syc' = if state py (fst disp) then ((-sy), dark blue) else (sy, dark red)

state :: Float -> Integer -> Bool
state sn d =
  (sn >=  fromInteger d/2) ||
  (sn <= -fromInteger d/2)


main :: IO ()
main = play window background fps initialState render handleKeys update
