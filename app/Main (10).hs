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
  = Figure {fgP :: Position, fgDg :: Degree, fgS :: Speed, fgT :: Type, fgC :: Color, fgWhs :: [Figure]}
  | Wheel {whP :: Position, whDg :: Degree, whT :: Type, whC :: Color, whP2 :: Position}
data Geometry = Angle Degree Geometry Geometry | Parallel [Geometry] | Linear [Geometry] | Line (Float,Float) deriving Show
--data Fi_Wh = P | D | S | T | С | Whs


fig `fPos` upd = fig {fgP = upd}
fig `fDeg` upd = fig {fgDg = upd}
fig `fSpd` upd = fig {fgS = upd}
fig `fTyp` upd = fig {fgT = upd}
fig `fCol` upd = fig {fgC = upd}
fig `fWhs` upd = fig {fgWhs = upd}

wheel `wPos` upd = wheel {whP = upd}
wheel `wDeg` upd = wheel {whDg = upd}
wheel `wTyp` upd = wheel {whT = upd}
wheel `wCol` upd = wheel {whC = upd}
wheel `wPos2` upd = wheel {whP2 = upd}

scene = mixColors 50 50 green orange
{-
disp :: Num a => (a, a)
disp = (900, 1600)
dr = dark red
ds = dark scene
-}
window :: Display
window = FullScreen

background :: Color
background = scene

fps :: Int
fps = 60

initialState :: [((Int,Int), Float, Type, Color, ([(Int,Int)], [Float], [Type], [Color]))] -> Figures
initialState =
  fmap
  (\((px,py),dg,t,c, (cpns,cdgs,cts,ccs)) ->
    Figure
      (toEnum px,toEnum py)
      dg
      (mulSV 100 $ unitVectorAtAngle $ degToRad dg)
      t
      c 
      ((\((cpx,cpy),cdg,ct,cc) ->
        Wheel
          (toEnum cpx, toEnum cpy)
          cdg
          ct
          cc
          (0,0))
       `fmap`
       (zip4 cpns cdgs cts ccs)) )


render :: Float -> Figures -> Picture
render angle lf =
  pictures $ whs_f $
    (\(Figure {fgP, fgDg, fgC, fgWhs}) ->
      (whs fgDg fgWhs, translate (fst fgP) (snd fgP) $ color (fgC) $ circleSolid 10))
    `fmap`
    lf

    where whs_f ((cs,f):xs) = cs++[f]++whs_f xs
          whs_f [] = []
          whs dg (Wheel {whDg, whC, whP2}:xs) =
            let (degreederx,degreedery) = (actcpx,actcpy) ^+^ (mulSV 10 $ unitVectorAtAngle $ degToRad (whDg+dg))
                (actcpx,actcpy) = whP2 in
            (translate actcpx actcpy $ rotate (-(whDg+dg)) $ color whC $ rectangleSolid 20 5):
            --(translate actcpx actcpy $ color ds $ circleSolid 3):
            (translate degreederx degreedery $ color white $ circleSolid 2):whs dg xs
          whs _ [] = []
          --figure Прямоугольник = rectangleSolid 10 10
          --figure Круг = circleSolid 10



handleKeys :: Float -> (Int,Int) -> Event -> Figures -> Figures

handleKeys angle (scx,scy) (EventKey (Char '[') Up _ _) game =
  (\(figure@(Figure {fgP, fgWhs})) ->
   let rotateCache = rotateWhs (-angle) fgWhs in
   figure
   `fWhs`
     (if fwhs_ifer (scx,scy) fgP rotateCache
      then fgWhs
      else
        (\((wheel@(Wheel {whDg})), cachen) ->
          wheel
          `wPos` cachen
          `wDeg` (whDg-angle))
        `fmap`
        (zip fgWhs rotateCache) ))
  `fmap`
  game

handleKeys angle (scx,scy) (EventKey (Char ']') Up _ _) game =
  (\(figure@(Figure {fgP, fgWhs})) ->
   let rotateCache = rotateWhs angle fgWhs in
   figure
   `fWhs`
     (if fwhs_ifer (scx,scy) fgP rotateCache
      then fgWhs
      else
        (\((wheel@(Wheel {whDg})), cachen) ->
          wheel
          `wPos` cachen
          `wDeg` (whDg+angle))
        `fmap`
        (zip fgWhs rotateCache) ))
  `fmap`
  game

handleKeys angle _ (EventKey (Char '-') Up _ _) game =
  (\(figure@(Figure {fgDg})) -> 
    let updg = fgDg-angle
        upsn = mulSV 100 $ unitVectorAtAngle $ degToRad updg in
    figure
    `fDeg` updg
    `fSpd` upsn)
  `fmap`
  game

handleKeys angle _ (EventKey (Char '=') Up _ _) game =
  (\(figure@(Figure {fgDg})) ->
    let updg = fgDg+angle
        upsn = mulSV 100 $ unitVectorAtAngle $ degToRad updg in
    figure
    `fDeg` updg
    `fSpd` upsn)
  `fmap`
  game

--handleKeys angle _ (EventKey (SpecialKey KeyTab) Up _ _) game = game
--handleKeys angle _ (EventKey (SpecialKey KeyCtrlL) Up _ _) game = game
handleKeys _ _ _ game = game

fwhs_ifer :: (Int,Int) -> (Float,Float) -> [(Float,Float)] -> Bool
fwhs_ifer (scx,scy) (x,y) ((xx,xy):xs) =
  if state (xx+x) scx || state (xy+y) scy
  then True
  else fwhs_ifer (scx,scy) (x,y) xs

fwhs_ifer _ _ [] = False

rotateWhs angle (Wheel{whP}:xs) = 
  rotateV (degToRad angle) whP:rotateWhs angle xs

rotateWhs _ [] = []

--(^*) :: Num a => (a,a) -> a -> (a,a)
--(^*) (a,b) p = (a*p,b*p)
(^+^) :: Num a => (a,a) -> (a,a) -> (a,a)
(^+^) (aa,ab) (ba,bb) = (aa+ba,ab+bb)



update :: (Int,Int) -> Float -> Figures -> Figures
update screen s =
  fmap
  (map_collision screen . map_position (Left s))

map_position :: Either Float (Float,Float) -> Figure -> Figure
map_position (Left s) (figure@(Figure {fgP, fgS, fgWhs})) =
  figure
  `fPos` (px', py')
  `fWhs`
    ((map_position (Right (px',py')))
     `fmap`
     fgWhs)
  
   where px' = (fst fgP) + (fst fgS) * s
         py' = (snd fgP) + (snd fgS) * s

map_position (Right (x,y)) (wheel@(Wheel {whP})) =
  wheel
  `wPos2` (fst whP,snd whP)
  `wPos2` ((fst whP)+x,(snd whP)+y)

map_collision :: (Int,Int) -> Figure -> Figure
map_collision (screenx,screeny) (figure@(Figure {fgP, fgDg, fgS, fgWhs})) =
  figure
  `fDeg` updg
  `fSpd` (upsx,upsy)

   where fp_fs st bc s =
           if st || bc
           then (True,(-s))
           else (False,s)
         fpsX = fp_fs (state (fst fgP) screenx) (find_collision True screenx fgWhs)
         fpsY = fp_fs (state (snd fgP) screeny) (find_collision False screeny fgWhs)
         ((logx,upsx),(logy,upsy)) = (fpsX (fst fgS), fpsY (snd fgS))
         updg =
           if logx || logy
           then radToDeg $ argV (upsx,upsy)
           else fgDg

find_collision True scn ((Wheel{whP2}):xs) =
  if state (fst whP2) scn
  then True
  else find_collision True scn xs

find_collision False scn ((Wheel{whP2}):xs) =
  if state (snd whP2) scn
  then True
  else find_collision False scn xs

find_collision _ _ [] = False

state :: Float -> Int -> Bool
state sn scn =
  sn >=  toEnum scn/2 ||
  sn <= -toEnum scn/2



main :: IO ()
main = do
  g <- getScreenSize
  [put, compon, ratio, num] <- replicateM 4 getLine

  let readput a b =
        if isJust $ fmap (*1) (readMaybe a)
        then read a
        else b
      numer = readput put 1
      enclosure = readput compon 3
      rater = readput ratio 5
      angle = readput num 10

      comrat = (div (fst g) rater, div (snd g) rater)

  rposits <- replicateM numer (randomRIO ( (div (-fst g) 2+fst comrat, div (-snd g) 2+snd comrat), (div (fst g) 2-fst comrat, div (snd g) 2-snd comrat) ))
  rdegrees <- replicateM numer (randomRIO ((0,360) :: (Float,Float)))
  rbools <- replicateM numer (randomIO :: IO Bool)
  rthrees <- replicateM numer (randomRIO ((1,3) :: (Int,Int)))
  rCposits <- replicateM (numer*enclosure) (randomRIO ((-fst comrat, -snd comrat), (fst comrat, snd comrat)))
  rCdegrees <- replicateM (numer*enclosure) (randomRIO ((0,360) :: (Float,Float)))
  rCbools <- replicateM (numer*enclosure) (randomIO :: IO Bool)
  rCthrees <- replicateM (numer*enclosure) (randomRIO ((1,3) :: (Int,Int)))

  play
   window background fps
   (initialState ( zip5 rposits rdegrees (rt rbools) (rc rthrees) (zip4 (ll enclosure rCposits) (ll enclosure rCdegrees) (ll enclosure (rt rCbools)) (ll enclosure (rc rCthrees))) ))
   (render angle) (handleKeys angle g) (update g)

    where rt (True:xs) = Круг:rt xs
          rt (False:xs) = Прямоугольник:rt xs
          rt [] = []

          rc (1:xs) = red:rc xs
          rc (2:xs) = green:rc xs
          rc (3:xs) = blue:rc xs
          rc [] = []

          ll n xer = take n xer:ll n (drop n xer)
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