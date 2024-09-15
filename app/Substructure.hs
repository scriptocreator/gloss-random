{-# LANGUAGE NamedFieldPuns #-}

module Substructure (substructure) where

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
import Data.List (zip5, zip3)


initialState :: [((Int,Int), (Int,Int), Type, Color, ([(Int,Int)], [Type], [Color]))] -> Figures
initialState =
  fmap
  (\((px,py),(sx,sy),t,c, (cpns,cts,ccs)) ->
     Figure
       (toEnum px,toEnum py)
       0
       (toEnum sx,toEnum sy)
       t
       c 
       ((\((cpx,cpy),ct,cc) ->
           Wheel
             (toEnum cpx, toEnum cpy)
             0
             ct
             cc
             (0,0))
        `fmap`
        (zip3 cpns cts ccs)) )


render :: Figures -> Picture
render lf =
  pictures $ fc_ft $ fmap
    (\(Figure {fgP,fgT,fgC,fgWhs}) ->
       (fcps fgWhs, translate (fst fgP) (snd fgP) $ color fgC $ tfigure fgT))
    lf

  where fc_ft ((cs,f):xs) = cs++[f]++fc_ft xs
        fc_ft [] = []

        fcps (Wheel {whT,whC,whP2}:xs) =
          (translate (fst whP2) (snd whP2) $ color whC $ tfigure whT):fcps xs
        fcps [] = []

        tfigure Прямоугольник = rectangleSolid 10 10
        tfigure Круг = circleSolid 10



handleKeys :: Float -> Event -> Figures -> Figures

handleKeys angle (EventKey (Char '-') Up _ _) game =
  (\(figure@(Figure {fgP})) ->
     figure
     `pos` (rotateV (degToRad (-angle)) fgP))
  `fmap`
  game

handleKeys angle (EventKey (Char '=') Up _ _) game =
  (\(figure@(Figure {fgP})) ->
     figure
     `pos` (rotateV (degToRad angle) fgP))
  `fmap`
  game

handleKeys _ _ game = game



update :: (Int,Int) -> Float -> Figures -> Figures
update g s =
  fmap
  (map_collision g . map_position (Left s))

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
map_collision (dx,dy) (figure@(Figure {fgP, fgDg, fgS, fgWhs})) =
  figure
  `spd` (fpsX, fpsY)

  where fp_fs st bc s =
          if st || bc
          then -s
          else s
        fpsX = fp_fs (state (fst fgP) dx) (find_collision True dx fgWhs) (fst fgS)
        fpsY = fp_fs (state (snd fgP) dy) (find_collision False dy fgWhs) (snd fgS)

find_collision True d (wheel:xs) =
  if state (fst $ whP2 wheel) d
  then True
  else find_collision True d xs

find_collision False d (wheel:xs) =
  if state (snd $ whP2 wheel) d
  then True
  else find_collision False d xs

find_collision _ _ [] = False



substructure :: ((Int,Int), Int, Int, (Int,Int), Float) -> IO ()
substructure (g, numer, enclosure, comrat, angle) = do

  rFPosits <- replicateM numer (randomRIO ( (div (-fst g) 2+fst comrat, div (-snd g) 2+snd comrat), (div (fst g) 2-fst comrat, div (snd g) 2-snd comrat) ))
  rFSpeeds <- replicateM numer (randomRIO ( (div (-400) 2, div (-400) 2), (div 400 2, div 400 2) ))
  rFbools <- replicateM numer (randomIO :: IO Bool)
  rFthrees <- replicateM numer (randomRIO ((1,3) :: (Int,Int)))
  rWPosits <- replicateM (numer*enclosure) (randomRIO ((-fst comrat, -snd comrat), (fst comrat, snd comrat)))
  rWbools <- replicateM (numer*enclosure) (randomIO :: IO Bool)
  rWthrees <- replicateM (numer*enclosure) (randomRIO ((1,3) :: (Int,Int)))

  let zipper =
        zip5
          rFPosits
          rFSpeeds
          rFTypes
          rFColors
          (zip3
             rangeWPosits
             rangeWTypes
             rangeWColors)

      rFTypes = rt rFbools
      rFColors = rc rFthrees
      rangeWPosits = rangeL enclosure rWPosits
      rangeWTypes = rangeL enclosure (rt rWbools)
      rangeWColors = rangeL enclosure (rc rWthrees)

  play
   window background fps (initialState zipper) render (handleKeys angle) (update g)

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
