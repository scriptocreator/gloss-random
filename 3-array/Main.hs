module Main (main) where

import Lib
import Prelude
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
import Data.Maybe
import Text.Read (readMaybe)
import Data.List (zip4)


type Position = (Float, Float)
type Speed = (Float, Float)
data Type = Прямоугольник | Круг
type Figures = [Figure]

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

initialState :: [(Int,Int)] -> [(Int,Int)] -> [Type] -> [Color] -> Figures
initialState rposit rspeed rtype rcolor =
  fmap (\((px,py),(sx,sy),t,c) -> Figure (toEnum px,toEnum py) (toEnum sx,toEnum sy) t c) (zip4  rposit rspeed rtype rcolor)


render :: Figures -> Picture
render lf =
  pictures (fmap (\(Figure (px,py) _ ff cdr) -> translate px py $ color cdr $ figure ff) lf)
   where
    figure Прямоугольник = rectangleSolid 10 10
    figure Круг = circleSolid 10


handleKeys :: Event -> Figures -> Figures
handleKeys _ game = game


update :: (Int,Int) -> Float -> Figures -> Figures
update g s = fmaplf
   where
    fmaplf = fmap (collision g . position s)

position :: Float -> Figure -> Figure
position s (Figure (px,py) (sx,sy) ff c) = Figure (px', py') (sx,sy) ff c
   where
    px' = px + sx * s
    py' = py + sy * s

collision :: (Int,Int) -> Figure -> Figure
collision (dx,dy) (Figure (px,py) (sx,sy) ff c) = Figure (px,py) (sxc', syc') ff c
   where
    sxc' = if state px dx then -sx else sx
    syc' = if state py dy then -sy else sy

state :: Float -> Int -> Bool
state sn d =
  (sn >=  toEnum d/2) ||
  (sn <= -toEnum d/2)


main :: IO ()
main = do
  put <- getLine
  let numer = if (isJust $ fmap (*1) (readMaybe put)) then read put else 1
  g <- getScreenSize
  rposit <- replicateM numer (randomRIO ((div (-fst g) 2, div (-snd g) 2), (div (fst g) 2, div (snd g) 2)))
  rspeed <- replicateM numer (randomRIO ((div (-400) 2, div (-400) 2), (div 400 2, div 400 2)))
  rbool <- replicateM numer (randomIO :: IO Bool)
  rthree <- replicateM numer (randomRIO ((1,3) :: (Int,Int)))
  --print g
  --print rp
  --print rs 
  play window background fps (initialState rposit rspeed (rt rbool) (rc rthree)) render handleKeys (update g)
   where
    rt (True:xs) = (Круг:rt xs)
    rt (False:xs) = (Прямоугольник:rt xs)
    rt [] = []

    rc (1:xs) = (red:rc xs)
    rc (2:xs) = (green:rc xs)
    rc (3:xs) = (blue:rc xs)
    rc [] = []
