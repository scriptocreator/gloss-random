{-# LANGUAGE NamedFieldPuns #-}

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

data Figure
  = Figure {fg'p :: Position, fg'dg :: Degree, fg's :: Speed, fg't :: Type, fg'c :: Color, fg'whs :: [Figure]}
  | Wheel {wh'p :: Position, wh'dg :: Degree, wh't :: Type, wh'c :: Color, wh'p2 :: Position}
data Geometry = Angle Degree Geometry Geometry | Parallel [Geometry] | Linear [Geometry] | Line (Float,Float) deriving Show


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
  fmap (\((px,py),dg,t,c, (cpns,cdgs,cts,ccs)) ->
    Figure
     { fg'p = (toEnum px,toEnum py)
     , fg'dg = dg
     , fg's = mulSV 100 $ unitVectorAtAngle $ degToRad dg
     , fg't = t
     , fg'c = c 
     , fg'whs = fmap (\((cpx,cpy),cdg,ct,cc) ->
       Wheel
        { wh'p = (toEnum cpx, toEnum cpy)
        , wh'dg = cdg
        , wh't = ct
        , wh'c = cc
        , wh'p2 = (0,0)
        }) (zip4 cpns cdgs cts ccs)})


render :: Float -> Figures -> Picture
render angle lf =
  pictures $ whs_f $
    (\(Figure {fg'p, fg'dg, fg'c, fg'whs}) ->
      (whs fg'dg fg'whs, translate (fst fg'p) (snd fg'p) $ color (fg'c) $ circleSolid 10))
    `fmap`
    lf

    where whs_f ((cs,f):xs) = cs++[f]++whs_f xs
          whs_f [] = []
          whs dg (Wheel {wh'dg, wh'c, wh'p2}:xs) =
            let (degreederx,degreedery) = (actcpx,actcpy) ^+^ (mulSV 10 $ unitVectorAtAngle $ degToRad (wh'dg+dg))
                (actcpx,actcpy) = wh'p2 in
            (translate actcpx actcpy $ rotate (-(wh'dg+dg)) $ color wh'c $ rectangleSolid 20 5):
            --(translate actcpx actcpy $ color ds $ circleSolid 3):
            (translate degreederx degreedery $ color white $ circleSolid 2):whs dg xs
          whs _ [] = []
          --figure Прямоугольник = rectangleSolid 10 10
          --figure Круг = circleSolid 10



handleKeys :: Float -> (Int,Int) -> Event -> Figures -> Figures

handleKeys angle (scx,scy) (EventKey (Char '[') Up _ _) game =
  fmap (\(figure@(Figure {fg'p, fg'whs})) ->
   let rotateCache = rotateWhs (-angle) fg'whs in
   figure {fg'whs =
     if fwhs_ifer (scx,scy) fg'p rotateCache
     then fg'whs
     else (fmap (\((wheel@(Wheel {wh'dg})), cachen) ->
             let upcdg = wh'dg-angle in
             wheel {wh'p = cachen, wh'dg = upcdg}) (zip fg'whs rotateCache)
           )}) game

handleKeys angle (scx,scy) (EventKey (Char ']') Up _ _) game =
  fmap (\(figure@(Figure {fg'p, fg'whs})) ->
   let rotateCache = rotateWhs angle fg'whs in
   figure {fg'whs =
     if fwhs_ifer (scx,scy) fg'p rotateCache
     then fg'whs
     else (fmap (\((wheel@(Wheel {wh'dg})), cachen) ->
            let upcdg = wh'dg+angle in
            wheel {wh'p = cachen, wh'dg = upcdg}) (zip fg'whs rotateCache)
           )}) game

handleKeys angle _ (EventKey (Char '-') Up _ _) game = fmap (\(figure@(Figure {fg'dg})) -> 
  let updg = fg'dg-angle
      upsn = mulSV 100 $ unitVectorAtAngle $ degToRad updg in
  figure {fg'dg = updg, fg's = upsn}) game
handleKeys angle _ (EventKey (Char '=') Up _ _) game = fmap (\(figure@(Figure {fg'dg})) ->
  let updg = fg'dg+angle
      upsn = mulSV 100 $ unitVectorAtAngle $ degToRad updg in
  figure {fg'dg = updg, fg's = upsn}) game
handleKeys _ _ _ game = game
handleKeys angle _ (EventKey (SpecialKey KeyTab) Up _ _) game = game
handleKeys angle _ (EventKey (SpecialKey KeyCtrlL) Up _ _) game = game

fwhs_ifer :: (Int,Int) -> (Float,Float) -> [(Float,Float)] -> Bool
fwhs_ifer (scx,scy) (x,y) ((xx,xy):xs) =
  if (state (xx+x) scx) || (state (xy+y) scy) then True else fwhs_ifer (scx,scy) (x,y) xs
fwhs_ifer _ _ [] = False

rotateWhs angle (wheel@(Wheel{wh'p}):xs) = 
  let angln = rotateV (degToRad angle) wh'p in
  angln:rotateWhs angle xs
rotateWhs _ [] = []

(^*) :: Num a => (a,a) -> a -> (a,a)
(^*) (a,b) p = (a*p,b*p)
(^+^) :: Num a => (a,a) -> (a,a) -> (a,a)
(^+^) (aa,ab) (ba,bb) = (aa+ba,ab+bb)



update :: (Int,Int) -> Float -> Figures -> Figures
update screen s = fmap (map_collision screen . map_position (Left s))

map_position :: Either Float (Float,Float) -> Figure -> Figure
map_position (Left s) (figure@(Figure {fg'p, fg's, fg'whs})) = figure {fg'p = (px', py'), fg'whs = fmap (map_position (Right (px',py'))) fg'whs}
   where
    px' = (fst fg'p) + (fst fg's) * s
    py' = (snd fg'p) + (snd fg's) * s

map_position (Right (x,y)) (wheel@(Wheel {wh'p})) =
  wheel {wh'p = (fst wh'p,snd wh'p), wh'p2 = ((fst wh'p)+x,(snd wh'p)+y)}

map_collision :: (Int,Int) -> Figure -> Figure
map_collision (screenx,screeny) (figure@(Figure {fg'p, fg'dg, fg's, fg'whs})) = figure {fg'dg = updg, fg's = (upsx,upsy)}
   where
    fp_fs st bc s = if st || bc then (True,(-s)) else (False,s)
    fps'x = fp_fs (state (fst fg'p) screenx) (find_collision True screenx fg'whs)
    fps'y = fp_fs (state (snd fg'p) screeny) (find_collision False screeny fg'whs)
    ((logx,upsx),(logy,upsy)) = (fps'x (fst fg's), fps'y (snd fg's))
    updg = if logx || logy then radToDeg $ argV (upsx,upsy) else fg'dg

find_collision True scn ((Wheel{wh'p2}):xs) =
  if state (fst wh'p2) scn then True else find_collision True scn xs
find_collision False scn ((Wheel{wh'p2}):xs) =
  if state (snd wh'p2) scn then True else find_collision False scn xs
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