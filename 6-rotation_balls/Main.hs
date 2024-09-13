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
import Data.List (zip5, zip3)


type Position = (Float, Float)
type Speed = (Float, Float)
data Type = Прямоугольник | Круг
type Figures = [Figure]
type Degree = Float

data Figure = Figure Position Degree Speed Type Color [Figure] | Wheel Position Type Color Position


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

initialState :: [((Int,Int), Float, Type, Color, ([(Int,Int)], [Type], [Color]))] -> Figures
initialState zipper =
  fmap (\((px,py),dg,t,c, (cpns,cts,ccs)) -> Figure (toEnum px,toEnum py) dg (unitVectorAtAngle $ degToRad dg) t c 
   (fmap (\((cpx,cpy),ct,cc) -> Wheel (toEnum cpx, toEnum cpy) ct cc (0,0)) (zip3 cpns cts ccs)))
    zipper


render :: Figures -> Picture
render lf =
  pictures $ fc_ft $ fmap (\(Figure (px,py) dg _ ft cdr cps) -> (fcps cps, translate px py $ color cdr $ figure ft)) lf
    where
     fc_ft ((cs,f):xs) = cs++[f]++fc_ft xs
     fc_ft [] = []
     fcps ((Wheel _ cft cc (actcpx,actcpy)):xs) = (translate actcpx actcpy $ color cc $ figure cft):fcps xs
     fcps [] = []
     figure Прямоугольник = rectangleSolid 10 10
     figure Круг = circleSolid 10


handleKeys :: Float -> Event -> Figures -> Figures
handleKeys angle (EventKey (Char '[') Up _ _) game =
  fmap (\(Figure pn dg sn ft c cps) -> Figure pn dg sn ft c 
    (fmap (\(Wheel cpn ct cc cpn2) -> Wheel (rotateV (degToRad (-angle)) cpn) ct cc cpn2) cps)) game
handleKeys angle (EventKey (Char ']') Up _ _) game =
  fmap (\(Figure pn dg sn ft c cps) -> Figure pn dg sn ft c 
    (fmap (\(Wheel cpn ct cc cpn2) -> Wheel (rotateV (degToRad angle) cpn) ct cc cpn2) cps)) game
--handleKeys angle (EventKey (Char ']') Up _ _) game = fmap (\(Figure pn sn ft c cps) -> Figure (rotateV (degToRad (-angle)) pn) sn ft c cps) game
handleKeys _ (EventKey (Char '-') Up _ _) game = fmap (\(Figure pn dg (sx,sy) ft c cps) -> Figure pn dg ((-sx),sy) ft c cps) game
handleKeys _ (EventKey (Char '=') Up _ _) game = fmap (\(Figure pn dg (sx,sy) ft c cps) -> Figure pn dg (sx,(-sy)) ft c cps) game
handleKeys _ (EventKey (Char '0') Up _ _) game = fmap (\(Figure pn dg (sx,sy) ft c cps) -> Figure pn dg ((-sx),(-sy)) ft c cps) game
handleKeys _ _ game = game


update :: (Int,Int) -> Float -> Figures -> Figures
update screen s = fmap (map_collision screen . map_position (Left s))

map_position :: Either Float (Float,Float) -> Figure -> Figure
map_position (Left s) (Figure (px,py) dg (sx,sy) ft c cps) = Figure (px', py') dg (sdgx,sdgy) ft c (fmap (map_position (Right (px',py'))) cps)
   where
    px' = px + sdgx * s
    py' = py + sdgy * s
    (sdgx,sdgy) = (unitVectorAtAngle $ degToRad dg) *^ 100

map_position (Right (x,y)) ((Wheel (cpx,cpy) ct cc _)) =
  Wheel (cpx,cpy) ct cc (cpx+x,cpy+y)

map_collision :: (Int,Int) -> Figure -> Figure
map_collision (screenx,screeny) (Figure (px,py) dg (sx,sy) ft c cps) = Figure (px,py) updg logsn ft c cps
   where
    fp_fs st bc s = if st || bc then -s else s
    fps'x = fp_fs (state px screenx) (find_collision True screenx cps)
    fps'y = fp_fs (state py screeny) (find_collision False screeny cps)
    logsn = (fps'x sx, fps'y sy)
    updg = radToDeg $ argV logsn

find_collision True scn ((Wheel _ _ _ (a,b)):xs) =
  let fcp_fcs = if state a scn then True else find_collision True scn xs in fcp_fcs
find_collision False scn ((Wheel _ _ _ (a,b)):xs) =
  let fcp_fcs = if state b scn then True else find_collision False scn xs in fcp_fcs
find_collision _ _ [] = False

state :: Float -> Int -> Bool
state sn scn =
  (sn >=  toEnum scn/2) ||
  (sn <= -toEnum scn/2)

(*^) :: Num a => (a,a) -> a -> (a,a) 
(*^) (a,b) p = (a*p,b*p)


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

  rposits <- replicateM numer (randomRIO ( ((div (-fst g) 2)+(fst comrat), (div (-snd g) 2)+(snd comrat)), ((div (fst g) 2)-(fst comrat), (div (snd g) 2)-(snd comrat)) ))
  rdegrees <- replicateM numer (randomRIO ((0,360) :: (Float,Float)))
  rbools <- replicateM numer (randomIO :: IO Bool)
  rthrees <- replicateM numer (randomRIO ((1,3) :: (Int,Int)))
  rCposits <- replicateM (numer*enclosure) (randomRIO (((-fst comrat), (-snd comrat)), (fst comrat, snd comrat)))
  rCbools <- replicateM (numer*enclosure) (randomIO :: IO Bool)
  rCthrees <- replicateM (numer*enclosure) (randomRIO ((1,3) :: (Int,Int)))

  play
   window background fps
   (initialState ( zip5 rposits rdegrees (rt rbools) (rc rthrees) (zip3 (ll enclosure rCposits) (ll enclosure (rt rCbools)) (ll enclosure (rc rCthrees))) ))
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