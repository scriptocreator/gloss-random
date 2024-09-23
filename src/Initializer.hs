module Initializer where

import Lib
import Prelude
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Color
import System.Random
import System.Random.Stateful
import Control.Monad
import Data.Maybe
import Data.List (zip3, zip4)


data Zipper
  = Zipper'Fo [((Int, Int), Float)]
  | Zipper'F [((Int, Int), Float, Type, Color)]
  | Zipper'FWs [((Int, Int), Float, Type, Color, ([(Int, Int)], [Type], [Color]))]
  | Zipper'FW [((Int, Int), Float, Type, Color, ([(Int, Int)], [Float], [Type], [Color]))]

data Initial
  = Initial'Fo ((Int, Int), Float)
  | Initial'F ([(Int, Int)], [Float], [Bool], [Int])
  | Initial'FWis ([(Int, Int)], [Float], [Bool], [Int], ([(Int, Int)], [Bool], [Int]))
  | Initial'FWs ([(Int, Int)], [Float], [Bool], [Int], ([(Int, Int)], [Bool], [Int]))
  | Initial'FW ([(Int, Int)], [Float], [Bool], [Int], ([(Int, Int)], [Float], [Bool], [Int]))


initialState :: PatternMatching
             -> Zipper
             -> Figures


initialState OneFigure (Zipper'Fo (((rpx,rpy),dg):xs)) =
  [Figure
     (toEnum rpx, toEnum rpy)
     dg
     (mulSV 100 $ unitVectorAtAngle $ degToRad dg)
     Круг
     (dark red)
     []]

initialState OnlyFigure (Zipper'F lf) =
  fmap
  (\((px,py),dg,t,c) ->
    Figure
      (toEnum px,toEnum py)
      dg
      (mulSV 100 $ unitVectorAtAngle $ degToRad dg)
      t
      c
      [])
  lf


initialState FigureAndStaticWheel (Zipper'FWs lf) =
  fmap
  (\((px,py),dg,t,c, (cpns,cts,ccs)) ->
     Figure
       (toEnum px,toEnum py)
       dg
       (mulSV 100 $ unitVectorAtAngle $ degToRad dg)
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
  lf


initialState FigureAndWheel (Zipper'FW lf) =
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
        (zip4 cpns cdgs cts ccs) ))
  lf


ioRandoms :: PatternMatching
          -> (Int,Int)
          -> Int
          -> Maybe Int
          -> Maybe (Int,Int)
          -> IO Initial

ioRandoms pm g numer ecs cr = do
  rFPositImageOne <- randomRIO ( (div (-fst g) 2, div (-snd g) 2), (div (fst g) 2, div (snd g) 2) )
  rFDegreeOne <- randomRIO ((0,360) :: (Float,Float))
  rFPositsImage <- replicateM numer (randomRIO ( (div (-fst g) 2, div (-snd g) 2), (div (fst g) 2, div (snd g) 2) ))
  rFPosits <- replicateM numer (randomRIO ( (div (-fst g) 2+fst comrat, div (-snd g) 2+snd comrat), (div (fst g) 2-fst comrat, div (snd g) 2-snd comrat) ))
  rFDegrees <- replicateM numer (randomRIO ((0,360) :: (Float,Float)))
  rFbools <- replicateM numer (randomIO :: IO Bool)
  rFthrees <- replicateM numer (randomRIO ((1,3) :: (Int,Int)))
  
  let iowheel = wheel (fromMaybe 0 ecs)
  (rWPosits, rWDegrees, rWbools, rWthrees) <- iowheel

  case pm of
    OneFigure -> return $ Initial'Fo (rFPositImageOne, rFDegreeOne)
    OnlyFigure -> return $ Initial'F (rFPositsImage, rFDegrees, rFbools, rFthrees)
    FigureImageAndStaticWheel -> return $ Initial'FWis (rFPositsImage, rFDegrees, rFbools, rFthrees, (rWPosits, rWbools, rWthrees))
    FigureAndStaticWheel -> return $ Initial'FWs (rFPosits, rFDegrees, rFbools, rFthrees, (rWPosits, rWbools, rWthrees))
    FigureAndWheel -> return $ Initial'FW (rFPosits, rFDegrees, rFbools, rFthrees, (rWPosits, rWDegrees, rWbools, rWthrees))

  where comrat = fromMaybe (0,0) cr
        wheel enclosure = do
          rWPosits <- replicateM (numer*enclosure) (randomRIO ((-fst comrat, -snd comrat), (fst comrat, snd comrat)))
          rWDegrees <- replicateM (numer*enclosure) (randomRIO ((0,360) :: (Float,Float)))
          rWbools <- replicateM (numer*enclosure) (randomIO :: IO Bool)
          rWthrees <- replicateM (numer*enclosure) (randomRIO ((1,3) :: (Int,Int)))
          
          return (rWPosits, rWDegrees, rWbools, rWthrees)

--ioInitial OnlyFigure = do
--  (ioFPosits, ioFDegrees, ioFbools, ioFthrees)
--ioInitial FigureAndStaticWheel []