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
  pictures $ fc_ft $ fmap (\(Figure (px,py) _ ft cdr cps) -> (fcps cps, translate px py $ color cdr $ figure ft)) lf
    where
     fc_ft ((cs,f):xs) = cs++[f]++fc_ft xs
     fc_ft [] = []
     fcps ((Component _ cft cc (actcpx,actcpy)):xs) = (translate actcpx actcpy $ color cc $ figure cft):fcps xs
     fcps [] = []
     figure Прямоугольник = rectangleSolid 10 10
     figure Круг = circleSolid 10


handleKeys :: Float -> Event -> Figures -> Figures
handleKeys angle (EventKey (Char '-') Up _ _) game = fmap (\(Figure pn sn ft c cps) -> Figure (rotateV (degToRad (-angle)) pn) sn ft c cps) game
handleKeys angle (EventKey (Char '=') Up _ _) game = fmap (\(Figure pn sn ft c cps) -> Figure (rotateV (degToRad angle) pn) sn ft c cps) game
handleKeys _ _ game = game


update :: (Int,Int) -> Float -> Figures -> Figures
update g s = fmap (map_collision g . map_position (Left s))

map_position :: Either Float (Float,Float) -> Figure -> Figure
map_position (Left s) (Figure (px,py) (sx,sy) ft c cps) = Figure (px', py') (sx,sy) ft c (fmap (map_position (Right (px',py'))) cps)
   where
    px' = px + sx * s
    py' = py + sy * s

map_position (Right (x,y)) ((Component (cpx,cpy) ct cc _)) =
  Component (cpx,cpy) ct cc (cpx+x,cpy+y)

map_collision :: (Int,Int) -> Figure -> Figure
map_collision (dx,dy) (Figure (px,py) (sx,sy) ft c cps) = Figure (px,py) (fps'x, fps'y) ft c cps
   where
    fp_fs p s d bc = if bc || (state p d) then -s else s
    fps'x = fp_fs px sx dx (find_collision True dx cps)
    fps'y = fp_fs py sy dy (find_collision False dy cps)

find_collision True d ((Component _ _ _ (a,b)):xs) =
      let fcp_fcs = if state a d then True else find_collision True d xs in fcp_fcs
find_collision False d ((Component _ _ _ (a,b)):xs) =
      let fcp_fcs = if state b d then True else find_collision False d xs in fcp_fcs
find_collision _ _ [] = False

state :: Float -> Int -> Bool
state sn d =
  (sn >=  toEnum d/2) ||
  (sn <= -toEnum d/2)


main :: IO ()
main = do
  g <- getScreenSize
  [put, compon, ratio, num] <- replicateM 4 getLine

  let readput a b = if (isJust $ fmap (*1) (readMaybe a)) then read a else b
      numer = readput put 1
      enclosure = readput compon 3
      rater = readput ratio 5
      angle = readput num 10

      comrat = (div (fst g) rater, div (snd g) rater)

  rposit <- replicateM numer (randomRIO ( ((div (-fst g) 2)+(fst comrat), (div (-snd g) 2)+(snd comrat)), ((div (fst g) 2)-(fst comrat), (div (snd g) 2)-(snd comrat)) ))
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