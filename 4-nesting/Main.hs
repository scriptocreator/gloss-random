module Main (main) where

import Lib
import Prelude
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
--import Graphics.UI.GLUT.Fonts as GLUTF
--import Graphics.UI.GLFW as GLFW
import System.Random
import System.Random.Stateful
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import Text.Read (readMaybe)
import Data.List (zip7, zip3)


type Position = (Float, Float)
type Speed = (Float, Float)
data Type = Прямоугольник | Круг
type Figures = [Figure]

data Figure = Figure Position Speed Type Color [Figure] | Component Position Type Color Position


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

initialState :: ([(Int,Int)], [(Int,Int)], [Type], [Color]) -> ([[(Int,Int)]], [[Type]], [[Color]]) -> Figures
initialState (rposit, rspeed, rtype, rcolor) (rCposit, rCtype, rCcolor) =
  fmap (\((px,py),(sx,sy),t,c, cpns,cts,ccs) -> Figure (toEnum px,toEnum py) (toEnum sx,toEnum sy) t c 
   (fmap (\((cpx,cpy),ct,cc) -> Component (toEnum cpx, toEnum cpy) ct cc (0,0)) (zip3 cpns cts ccs)))
    (zip7 rposit rspeed rtype rcolor rCposit rCtype rCcolor)


render :: Figures -> Picture
render lf =
  pictures $ fc_ff $ fmap (\(Figure (px,py) _ ff cdr cps) -> (fcps cps, translate px py $ color cdr $ figure ff)) lf
    where
     fc_ff ((cs,f):xs) = cs++[f]++fc_ff xs
     fc_ff [] = []
     fcps ((Component _ cff cc (actcpx,actcpy)):xs) = (translate actcpx actcpy $ color cc $ figure cff):fcps xs
     fcps [] = []
     figure Прямоугольник = rectangleSolid 10 10
     figure Круг = circleSolid 10


handleKeys :: Float -> Event -> Figures -> Figures
handleKeys angle (EventKey (Char '-') Up _ _) game = fmap (\(Figure pn sn ff c cps) -> Figure (rotateV (degToRad (-angle)) pn) sn ff c cps) game
handleKeys angle (EventKey (Char '=') Up _ _) game = fmap (\(Figure pn sn ff c cps) -> Figure (rotateV (degToRad angle) pn) sn ff c cps) game
handleKeys _ _ game = game


update :: (Int,Int) -> Float -> Figures -> Figures
update g s = fmap (collision g . position s)    

position :: Float -> Figure -> Figure
position s (Figure (px,py) (sx,sy) ff c cps) = Figure (px', py') (sx,sy) ff c (fcps cps)
   where
    px' = px + sx * s
    py' = py + sy * s
    fcps ((Component (cpx,cpy) ct cc (a,b)):xs) =
      Component (cpx,cpy) ct cc (cpx+px',cpy+py'):(fcps xs)
    fcps [] = []

collision :: (Int,Int) -> Figure -> Figure
collision (dx,dy) (Figure (px,py) (sx,sy) ff c cps) = Figure (px,py) (sxc', syc') ff c cps
   where
    sxc' = if state px dx then -sx else sx
    syc' = if state py dy then -sy else sy

state :: Float -> Int -> Bool
state sn d =
  (sn >=  toEnum d/2) ||
  (sn <= -toEnum d/2)


main :: IO ()
main = do
  g <- getScreenSize
  [put, compon, num, ratio] <- replicateM 4 getLine

  let readput a b = if (isJust $ fmap (*1) (readMaybe a)) then read a else b
      numer = readput put 1
      enclosure = readput compon 3
      angle = readput num 10
      rater = readput ratio 5

      comrat = (div (fst g) rater, div (snd g) rater)

  rposit <- replicateM numer (randomRIO ((div (-fst g) 2, div (-snd g) 2), (div (fst g) 2, div (snd g) 2)))
  rspeed <- replicateM numer (randomRIO ((div (-400) 2, div (-400) 2), (div 400 2, div 400 2)))
  rbool <- replicateM numer (randomIO :: IO Bool)
  rthree <- replicateM numer (randomRIO ((1,3) :: (Int,Int)))
  rCposit <- replicateM (numer*enclosure) (randomRIO (((-fst comrat), (-snd comrat)), (fst comrat, snd comrat)))
  rCbool <- replicateM (numer*enclosure) (randomIO :: IO Bool)
  rCthree <- replicateM (numer*enclosure) (randomRIO ((1,3) :: (Int,Int)))

  play
   window background fps
   (initialState (rposit, rspeed, rt rbool, rc rthree) (ll enclosure rCposit, ll enclosure (rt rCbool), ll enclosure (rc rCthree)))
   render (handleKeys angle) (update g)
    where
     rt (True:xs) = (Круг:rt xs)
     rt (False:xs) = (Прямоугольник:rt xs)
     rt [] = []

     rc (1:xs) = (red:rc xs)
     rc (2:xs) = (green:rc xs)
     rc (3:xs) = (blue:rc xs)
     rc [] = []

     ll n (x:xs) = (take n (x:xs)):ll n (drop n (x:xs))
     ll _ [] = []

{- Работающая лень (для лени нужен оператор «:»).
lz2 n (x:xs) = (x*n):lz2 n xs
lz2 _ [] = []

lz :: Int -> [[Int]] -> [[Int]]
lz n (x:xs) = (lz2 n x):(lz n xs)
lz _ [] = []

-- Ориентация координат.
• Горизонтальная ось X - 
• 
-}