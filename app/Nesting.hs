{-# LANGUAGE NamedFieldPuns #-}

module Nesting (nesting) where

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
  pictures $ fc_ff $ fmap
    (\(Figure {fgP,fgT,fgC,fgWhs}) ->
       (fcps fgWhs, translate (fst fgP) (snd fgP) $ color fgC $ tfigure fgT))
    lf

  where fc_ff ((cs,f):xs) = cs++[f]++fc_ff xs
        fc_ff [] = []

        fcps (Wheel {whT,whC,whP2}:xs) = (translate (fst whP2) (snd whP2) $ color whC $ tfigure whT):fcps xs
        fcps [] = []

        tfigure Прямоугольник = rectangleSolid 10 10
        tfigure Круг = circleSolid 10



handleKeys :: Float -> (Int,Int) -> Event -> Figures -> Figures

handleKeys angle (scx,scy) (EventKey (Char '-') Up _ _) game =
  (\(figure@(Figure {fgP})) ->
     let offset = rotateV (degToRad (-angle)) fgP
     in
     figure
     `pos`
       if state (fst offset) scx || state (snd offset) scy
       then fgP
       else offset)
  `fmap`
  game

handleKeys angle (scx,scy) (EventKey (Char '=') Up _ _) game =
  (\(figure@(Figure {fgP})) ->
     let offset = rotateV (degToRad angle) fgP
     in
     figure
     `pos`
       if state (fst offset) scx || state (snd offset) scy
       then fgP
       else offset)
  `fmap`
  game

handleKeys _ _ _ game = game



update :: (Int,Int) -> Float -> Figures -> Figures
update g s = fmap (collision g . position s)    

position :: Float -> Figure -> Figure
position s (figure@(Figure {fgP,fgS,fgWhs})) =
  figure
  `pos` (px', py')
  `whs` (fcps fgWhs)

  where px' = fst fgP + fst fgS * s
        py' = snd fgP + snd fgS * s

        fcps (wheel@(Wheel {whP}):xs) =
          (wheel
           `pos2`
           (fst whP+px',snd whP+py')):fcps xs
        fcps [] = []

collision :: (Int,Int) -> Figure -> Figure
collision (dx,dy) (figure@(Figure {fgP,fgS})) =
  figure
  `spd` (sxc', syc')

  where sc' p d s =
          if state p d
          then -s
          else s
        sxc' = sc' (fst fgP) dx (fst fgS)
        syc' = sc' (snd fgP) dy (snd fgS)



nesting :: ((Int,Int), Int, Int, (Int,Int), Float) -> IO ()
nesting (g, numer, enclosure, comrat, angle) = do

  rFPosits <- replicateM numer (randomRIO ( (div (-fst g) 2, div (-snd g) 2), (div (fst g) 2, div (snd g) 2) ))
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
