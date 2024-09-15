{-# LANGUAGE NamedFieldPuns #-}

module Wheel (wheel) where

import Lib
import Prelude
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
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
       (unitVectorAtAngle $ degToRad dg)
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
        (zip4 cpns cdgs cts ccs) ))


render :: Figures -> Picture
render lf =
  pictures $ whs_f $ fmap
    (\(Figure {fgP,fgC,fgWhs}) ->
       (whs fgWhs, translate (fst fgP) (snd fgP) $ color fgC $ circleSolid 10))
    lf

  where whs_f ((cs,f):xs) = cs++[f]++whs_f xs
        whs_f [] = []

        whs (Wheel {whDg,whT,whC,whP2}:xs) =
          (translate (fst whP2) (snd whP2) $ rotate whDg $ color whC $ rectangleSolid 10 5):whs xs
        whs [] = []



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
         (\(fwheel, cachen) ->
            fwheel
            `pos` cachen)
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
         (\(fwheel, cachen) ->
            fwheel
            `pos` cachen)
         `fmap`
         (zip fgWhs rotateCache) )
  `fmap`
  game

handleKeys _ _ (EventKey (Char '-') Up _ _) game =
  (\figure@(Figure {fgS}) ->
     figure
     `spd` (-fst fgS,snd fgS))
  `fmap`
  game

handleKeys _ _ (EventKey (Char '=') Up _ _) game =
  (\figure@(Figure {fgS}) ->
     figure
     `spd` (fst fgS,-snd fgS))
  `fmap`
  game

handleKeys _ _ (EventKey (Char '0') Up _ _) game =
  (\figure@(Figure {fgS}) ->
     figure
     `spd` (-fst fgS,-snd fgS))
  `fmap`
  game

handleKeys _ _ _ game = game

fwhs_ifer :: (Int,Int) -> (Float,Float) -> [(Float,Float)] -> Bool
fwhs_ifer (scx,scy) (x,y) ((xx,xy):xs) =
  if state (xx+x) scx || state (xy+y) scy
  then True
  else fwhs_ifer (scx,scy) (x,y) xs

fwhs_ifer _ _ [] = False

rotateWhs angle (fwheel:xs) =
  rotateV (degToRad angle) (whP fwheel):rotateWhs angle xs

rotateWhs _ [] = []



update :: (Int,Int) -> Float -> Figures -> Figures
update screen s =
  fmap
  (map_collision screen . map_position (Left s))

map_position :: Either Float (Float,Float) -> Figure -> Figure
map_position (Left s) (figure@(Figure {fgP, fgDg, fgS, fgWhs})) =
  figure
  `pos` (px', py')
  `spd` sdgn
  `whs`
    ((map_position (Right (px',py')))
     `fmap`
     fgWhs)

  where px' = fst fgP + fst sdgn * s
        py' = snd fgP + snd sdgn * s
        sdgn = (unitVectorAtAngle $ degToRad fgDg) ^* 100

map_position (Right (x,y)) (fwheel@(Wheel {whP})) =
  fwheel
  `pos2` (fst whP+x,snd whP+y)

map_collision :: (Int,Int) -> Figure -> Figure
map_collision (screenx,screeny) (figure@(Figure {fgP, fgDg, fgS, fgWhs})) =
  figure
  `deg` updg
  `spd` logsn

  where fp_fs st bc s =
          if st || bc
          then -s
          else s
        fpsX = fp_fs (state (fst fgP) screenx) (find_collision True screenx fgWhs)
        fpsY = fp_fs (state (snd fgP) screeny) (find_collision False screeny fgWhs)
        logsn = (fpsX (fst fgS), fpsY (snd fgS))
        updg = radToDeg $ argV logsn

find_collision True scn (fwheel:xs) =
  if state (fst $ whP2 fwheel) scn
  then True
  else find_collision True scn xs

find_collision False scn (fwheel:xs) =
  if state (snd $ whP2 fwheel) scn
  then True
  else find_collision False scn xs

find_collision _ _ [] = False



wheel :: ((Int,Int), Int, Int, (Int,Int), Float) -> IO ()
wheel (g, numer, enclosure, comrat, angle) = do

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
   window background fps (initialState zipper) render (handleKeys angle g) (update g)

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
