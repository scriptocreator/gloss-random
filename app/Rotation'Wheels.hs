{-# LANGUAGE NamedFieldPuns #-}

module Rotation'Wheels (rotation'wheels) where

import Lib
import Prelude
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
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
import Data.List (zip5, zip4)


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
  pictures $ whs_f $ fmap
    (\(Figure {fgP, fgDg, fgC, fgWhs}) ->
       (whs fgDg fgWhs, translate (fst fgP) (snd fgP) $ color fgC $ circleSolid 10))
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



handleKeys :: Float -> (Int,Int) -> Event -> Figures -> Figures
{-
handleKeys angle scn (EventKey (Char 'х') Up a b) figure =
  handleKeys angle scn (EventKey (Char '[') Up a b) figure

handleKeys angle scn (EventKey (Char 'ъ') Up a b) figure =
  handleKeys angle scn (EventKey (Char ']') Up a b) figure
-}
handleKeys angle (scx,scy) (EventKey (Char '[') Up _ _) game =
  (\(figure@(Figure {fgP, fgWhs})) ->
     let rotateCache = rotateWhs (-angle) fgWhs in
     figure 
     `whs`
       if fwhs_ifer (scx,scy) fgP rotateCache
       then fgWhs
       else
         (\(wheel, cachen) ->
            wheel
            `pos` cachen
            `deg` (whDg wheel-angle))
         `fmap`
         (zip fgWhs rotateCache) )
  `fmap`
  game

handleKeys angle (scx,scy) (EventKey (Char ']') Up _ _) game =
  (\(figure@(Figure {fgP, fgWhs})) ->
     let rotateCache = rotateWhs angle fgWhs in
     figure 
     `whs`
       if fwhs_ifer (scx,scy) fgP rotateCache
       then fgWhs
       else
         (\(wheel, cachen) ->
            wheel
            `pos` cachen
            `deg` (whDg wheel+angle))
         `fmap`
         (zip fgWhs rotateCache) )
  `fmap`
  game

handleKeys angle _ (EventKey (Char '-') Up _ _) game =
  (\figure -> 
     let updg = fgDg figure-angle
         upsn = mulSV 100 $ unitVectorAtAngle $ degToRad updg in
     figure
     `deg` updg
     `spd` upsn)
  `fmap`
  game

handleKeys angle _ (EventKey (Char '=') Up _ _) game =
  (\figure -> 
     let updg = fgDg figure+angle
         upsn = mulSV 100 $ unitVectorAtAngle $ degToRad updg in
     figure
     `deg` updg
     `spd` upsn)
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

rotateWhs angle (wheel:xs) =
  rotateV (degToRad angle) (whP wheel):rotateWhs angle xs

rotateWhs _ [] = []



update :: (Int,Int) -> Float -> Figures -> Figures
update screen s =
  fmap
  (map_collision screen . map_position (Left s))

map_position :: Either Float (Float,Float) -> Figure -> Figure
map_position (Left s) (figure@(Figure {fgP, fgS, fgWhs})) =
  figure
  `pos` (px', py')
  `whs`
    ((map_position (Right (px',py')))
     `fmap`
     fgWhs)

  where px' = fst fgP + fst fgS * s
        py' = snd fgP + snd fgS * s

map_position (Right (x,y)) (wheel@(Wheel {whP})) =
  wheel
  `pos2` (fst whP+x,snd whP+y)

map_collision :: (Int,Int) -> Figure -> Figure
map_collision (screenx,screeny) (figure@(Figure {fgP, fgDg, fgS, fgWhs})) =
  figure
  `deg` updg
  `spd` (upsx,upsy)

  where fp_fs st bc s =
          if st || bc
          then (True,(-s))
          else (False,s)

        fpsX = fp_fs (state (fst fgP) screenx) (find_collision True screenx fgWhs)
        fpsY = fp_fs (state (snd fgP) screeny) (find_collision False screeny fgWhs)
        (logx,upsx) = fpsX (fst fgS)
        (logy,upsy) = fpsY (snd fgS)

        updg =
          if logx || logy
          then radToDeg $ argV (upsx,upsy)
          else fgDg

find_collision True scn (wheel:xs) =
  if state (fst $ whP2 wheel) scn
  then True
  else find_collision True scn xs

find_collision False scn (wheel:xs) =
  if state (snd $ whP2 wheel) scn
  then True
  else find_collision False scn xs

find_collision _ _ [] = False



rotation'wheels :: ((Int,Int), Int, Int, (Int,Int), Float) -> IO ()
rotation'wheels (g, numer, enclosure, comrat, angle) = do

  rFPosits <- replicateM numer (randomRIO ( (div (-fst g) 2+fst comrat, div (-snd g) 2+snd comrat), (div (fst g) 2-fst comrat, div (snd g) 2-snd comrat) ))
  rFDegrees <- replicateM numer (randomRIO ((0,360) :: (Float,Float)))
  rFbools <- replicateM numer (randomIO :: IO Bool)
  rFthrees <- replicateM numer (randomRIO ((1,3) :: (Int,Int)))
  rWPosits <- replicateM (numer*enclosure) (randomRIO ((-fst comrat, -snd comrat), (fst comrat, snd comrat)))
  rWDegrees <- replicateM (numer*enclosure) (randomRIO ((0,360) :: (Float,Float)))
  rWbools <- replicateM (numer*enclosure) (randomIO :: IO Bool)
  rWthrees <- replicateM (numer*enclosure) (randomRIO ((1,3) :: (Int,Int)))

  let zipper =
        zip5
          rFPosits
          rFDegrees
          rFTypes
          rFColors
            (zip4
               rangeWPosits
               rangeWDegrees
               rangeWTypes
               rangeWColors)

      rFTypes = rt rFbools
      rFColors = rc rFthrees
      rangeWPosits = rangeL enclosure rWPosits
      rangeWDegrees = rangeL enclosure rWDegrees
      rangeWTypes = rangeL enclosure (rt rWbools)
      rangeWColors = rangeL enclosure (rc rWthrees)

  play
   window background fps (initialState zipper) (render angle) (handleKeys angle g) (update g)

  where rt (True:xs) = Круг:rt xs
        rt (False:xs) = Прямоугольник:rt xs
        rt [] = []

        rc (1:xs) = red:rc xs
        rc (2:xs) = green:rc xs
        rc (3:xs) = blue:rc xs
        rc (_:xs) = rc xs
        rc [] = []

        rangeL n xer = take n xer:rangeL n (drop n xer)
        rangeL _ [] = []
