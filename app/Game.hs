{-# LANGUAGE NamedFieldPuns #-}

module Game (game) where

import Lib
import Initializer
import PositAndSpeed
import HandleKeys
import Render

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
import Data.List (zip3, zip4, zip5)
import Data.Maybe (fromMaybe)


update :: PatternMatching
       -> PatternMatching
       -> (Int,Int)
       -> Float
       -> Figures
       -> Figures

update pmp pmc dn s =
  fmap
  (map_collision pmc dn . map_position pmp (Left s))



game :: Minigames
     -> (Int,Int)
     -> (Maybe Int, Maybe Int, Maybe (Int,Int), Maybe Float)
     -> IO ()



game Ball'Control dn _ = do

  (Initial'Fo
    (rFPosit
    ,rFDegree)) <- ioRandoms OneFigure dn 1 Nothing Nothing

  play
   window background fps
   (initialState OneFigure (Zipper'Fo [(rFPosit, rFDegree)]))
   (render OneFigure Nothing)
   (handleKeys OneFigure 0.0 dn)
   (update OnlyFigure OnlyFigure dn)



game Array dn (Just numer, _, _, Just angle) = do

  (Initial'F
    (rFPosits
    ,rFDegrees
    ,rFbools
    ,rFthrees)) <- ioRandoms OnlyFigure dn numer Nothing Nothing

  let zipper =
        zip4
          rFPosits
          rFDegrees
          rFTypes
          rFColors

      rFTypes = rt rFbools
      rFColors = rc rFthrees

  play
   window background fps
   (initialState OnlyFigure (Zipper'F zipper))
   (render OnlyFigure Nothing)
   (handleKeys OnlyFigure angle dn)
   (update OnlyFigure OnlyFigure dn)



game Nesting dn (Just numer, Just enclosure, Just comrat, Just angle) = do

  (Initial'FWis
    (rFPosits
    ,rFDegrees
    ,rFbools
    ,rFthrees
    ,(rWPosits
     ,rWbools
     ,rWthrees))) <- ioRandoms FigureImageAndStaticWheel dn numer (Just enclosure) (Just comrat)

  let zipper =
        zip5
          rFPosits
          rFDegrees
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
   window background fps
   (initialState FigureAndStaticWheel (Zipper'FWs zipper))
   (render FigureImageAndStaticWheel_OrStaticWheel Nothing)
   (handleKeys OnlyFigure angle dn)
   (update FigureAndStaticWheel OnlyFigure dn)



game Substructure dn (Just numer, Just enclosure, Just comrat, Just angle) = do

  (Initial'FWis
    (rFPosits
    ,rFDegrees
    ,rFbools
    ,rFthrees
    ,(rWPosits
     ,rWbools
     ,rWthrees))) <- ioRandoms FigureImageAndStaticWheel dn numer (Just enclosure) (Just comrat)

  let zipper =
        zip5
          rFPosits
          rFDegrees
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
   window background fps
   (initialState FigureAndStaticWheel (Zipper'FWs zipper))
   (render FigureImageAndStaticWheel_OrStaticWheel Nothing)
   (handleKeys OnlyFigure angle dn)
   (update FigureAndWheel OnlyFigure dn)



game Rotation'Balls dn (Just numer, Just enclosure, Just comrat, Just angle) = do

  (Initial'FWs
    (rFPosits
    ,rFDegrees
    ,rFbools
    ,rFthrees
    ,(rWPosits
     ,rWbools
     ,rWthrees))) <- ioRandoms FigureAndStaticWheel dn numer (Just enclosure) (Just comrat)

  let zipper =
        zip5
          rFPosits
          rFDegrees
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
   window background fps
   (initialState FigureAndStaticWheel (Zipper'FWs zipper))
   (render FigureImageAndStaticWheel_OrStaticWheel Nothing)
   (handleKeys FigureAndStaticWheel angle dn)
   (update FigureAndWheel FigureAndWheel_OrStaticWheel dn)



game Wheels dn (Just numer, Just enclosure, Just comrat, Just angle) = do

  (Initial'FW
    (rFPosits
    ,rFDegrees
    ,rFbools
    ,rFthrees
    ,(rWPosits
     ,rWDegrees
     ,rWbools
     ,rWthrees))) <- ioRandoms FigureAndWheel dn numer (Just enclosure) (Just comrat)

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
   window background fps
   (initialState FigureAndWheel (Zipper'FW zipper))
   (render FigureAndWheel Nothing)
   (handleKeys FigureAndStaticWheel angle dn)
   (update FigureAndWheel FigureAndWheel_OrStaticWheel dn)



game Rotation'Wheels dn (Just numer, Just enclosure, Just comrat, Just angle) = do

  (Initial'FW
    (rFPosits
    ,rFDegrees
    ,rFbools
    ,rFthrees
    ,(rWPosits
     ,rWDegrees
     ,rWbools
     ,rWthrees))) <- ioRandoms FigureAndWheel dn numer (Just enclosure) (Just comrat)

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
   window background fps
   (initialState FigureAndWheel (Zipper'FW zipper))
   (render FigureAndWheelAndVectors (Just angle))
   (handleKeys FigureAndWheel angle dn)
   (update FigureAndWheel FigureAndWheel_OrStaticWheel dn)



rt (True:xs) = Круг:rt xs
rt (False:xs) = Прямоугольник:rt xs
rt [] = []

rc (1:xs) = red:rc xs
rc (2:xs) = green:rc xs
rc (3:xs) = blue:rc xs
rc (_:xs) = rc xs
rc [] = []
