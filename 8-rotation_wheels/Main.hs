module Main (main) where

import Lib
import Prelude
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
--import Graphics.Gloss.Data.Point.Arithmetic as V ((+))
--import Graphics.UI.GLUT.Fonts as GLUTF
--import Graphics.UI.GLFW as GLFW
import System.Random
import System.Random.Stateful
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import Text.Read (readMaybe)
import Data.List (zip5, zip4)


type Position = (Float, Float)
type Speed = (Float, Float)
data Type = Прямоугольник | Круг
type Figures = [Figure]
type Degree = Float

data Figure = Figure Position Degree Speed Type Color [Figure] | Wheel Position Degree Type Color Position


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

initialState :: [((Int,Int), Float, Type, Color, ([(Int,Int)], [Float], [Type], [Color]))] -> Figures
initialState =
  fmap (\((px,py),dg,t,c, (cpns,cdgs,cts,ccs)) -> Figure (toEnum px,toEnum py) dg (mulSV 100 $ unitVectorAtAngle $ degToRad dg) t c 
   ( fmap (\((cpx,cpy),cdg,ct,cc) -> Wheel (toEnum cpx, toEnum cpy) cdg ct cc (0,0)) (zip4 cpns cdgs cts ccs) ))


render :: Float -> Figures -> Picture
render angle lf =
  pictures $ whs_f $ fmap (\(Figure (px,py) dg _ ft cdr cps) -> (whs dg cps, translate px py $ color cdr $ circleSolid 10)) lf
    where
     whs_f ((cs,f):xs) = cs++[f]++whs_f xs
     whs_f [] = []
     whs dg ((Wheel _ cdg cft cc (actcpx,actcpy)):xs) =
       let (degreederx,degreedery) = (actcpx,actcpy) ^+^ (mulSV 10 $ unitVectorAtAngle $ degToRad (cdg+dg))  in
       (translate actcpx actcpy $ rotate (-(cdg+dg)) $ color cc $ rectangleSolid 20 5):
       --(translate actcpx actcpy $ color ds $ circleSolid 3):
       (translate degreederx degreedery $ color white $ circleSolid 2):whs dg xs
     whs _ [] = []
     --figure Прямоугольник = rectangleSolid 10 10
     --figure Круг = circleSolid 10



handleKeys :: Float -> (Int,Int) -> Event -> Figures -> Figures

handleKeys angle (scx,scy) (EventKey (Char '[') Up _ _) game =
  fmap (\(Figure pn dg sn ft c cps) ->
   let rotateCache = rotateWhs (-angle) cps in
   Figure pn dg sn ft c
     (if fwhs_ifer (scx,scy) pn rotateCache
      then cps
      else (fmap (\((Wheel cpn cdg ct cc cpn2), cachen) ->
              let upcdg = cdg-angle in
              Wheel cachen upcdg ct cc cpn2) (zip cps rotateCache)
           ))) game

handleKeys angle (scx,scy) (EventKey (Char ']') Up _ _) game =
  fmap (\(Figure pn dg sn ft c cps) ->
   let rotateCache = rotateWhs angle cps in
   Figure pn dg sn ft c
     (if fwhs_ifer (scx,scy) pn rotateCache
      then cps
      else (fmap (\((Wheel cpn cdg ct cc cpn2), cachen) ->
             let upcdg = cdg+angle in
             Wheel cachen upcdg ct cc cpn2) (zip cps rotateCache)
           ))) game

handleKeys angle _ (EventKey (Char '-') Up _ _) game = fmap (\(Figure pn dg sn ft c cps) -> 
  let updg = dg-angle
      upsn = mulSV 100 $ unitVectorAtAngle $ degToRad updg in
  Figure pn updg upsn ft c cps) game
handleKeys angle _ (EventKey (Char '=') Up _ _) game = fmap (\(Figure pn dg sn ft c cps) ->
  let updg = dg+angle
      upsn = mulSV 100 $ unitVectorAtAngle $ degToRad updg in
  Figure pn updg upsn ft c cps) game
handleKeys _ _ _ game = game

fwhs_ifer :: (Int,Int) -> (Float,Float) -> [(Float,Float)] -> Bool
fwhs_ifer (scx,scy) (x,y) ((xx,xy):xs) =
  if (state (xx+x) scx) || (state (xy+y) scy) then True else fwhs_ifer (scx,scy) (x,y) xs
fwhs_ifer _ _ [] = False

rotateWhs angle (Wheel cpn _ _ _ _:xs) = 
  let angln = rotateV (degToRad angle) cpn in
  angln:rotateWhs angle xs
rotateWhs _ [] = []

(^*) :: Num a => (a,a) -> a -> (a,a)
(^*) (a,b) p = (a*p,b*p)
(^+^) :: Num a => (a,a) -> (a,a) -> (a,a)
(^+^) (aa,ab) (ba,bb) = (aa+ba,ab+bb)



update :: (Int,Int) -> Float -> Figures -> Figures
update screen s = fmap (map_collision screen . map_position (Left s))

map_position :: Either Float (Float,Float) -> Figure -> Figure
map_position (Left s) (Figure (px,py) dg (sx,sy) ft c cps) = Figure (px', py') dg (sx,sy) ft c (fmap (map_position (Right (px',py'))) cps)
   where
    px' = px + sx * s
    py' = py + sy * s

map_position (Right (x,y)) ((Wheel (cpx,cpy) cdg ct cc _)) =
  Wheel (cpx,cpy) cdg ct cc (cpx+x,cpy+y)

map_collision :: (Int,Int) -> Figure -> Figure
map_collision (screenx,screeny) (Figure (px,py) dg (sx,sy) ft c cps) = Figure (px,py) updg (upsx,upsy) ft c cps
   where
    fp_fs st bc s = if st || bc then (True,(-s)) else (False,s)
    fps'x = fp_fs (state px screenx) (find_collision True screenx cps)
    fps'y = fp_fs (state py screeny) (find_collision False screeny cps)
    ((logx,upsx),(logy,upsy)) = (fps'x sx, fps'y sy)
    updg = if logx || logy then radToDeg $ argV (upsx,upsy) else dg

find_collision True scn ((Wheel _ _ _ _ (a,b)):xs) =
  let fcp_fcs = if state a scn then True else find_collision True scn xs in fcp_fcs
find_collision False scn ((Wheel _ _ _ _ (a,b)):xs) =
  let fcp_fcs = if state b scn then True else find_collision False scn xs in fcp_fcs
find_collision _ _ [] = False

state :: Float -> Int -> Bool
state sn scn =
  (sn >=  toEnum scn/2) ||
  (sn <= -toEnum scn/2)



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
  rCdegrees <- replicateM (numer*enclosure) (randomRIO ((0,360) :: (Float,Float)))
  rCbools <- replicateM (numer*enclosure) (randomIO :: IO Bool)
  rCthrees <- replicateM (numer*enclosure) (randomRIO ((1,3) :: (Int,Int)))

  play
   window background fps
   (initialState ( zip5 rposits rdegrees (rt rbools) (rc rthrees) (zip4 (ll enclosure rCposits) (ll enclosure rCdegrees) (ll enclosure (rt rCbools)) (ll enclosure (rc rCthrees))) ))
   (render angle) (handleKeys angle g) (update g)
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