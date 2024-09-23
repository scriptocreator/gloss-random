{-# LANGUAGE NamedFieldPuns #-}

module Render where

import Lib
import Prelude
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle


render :: PatternMatching
       -> Maybe Float
       -> Figures
       -> Picture


render OneFigure _ (figure@(Figure {fgP, fgT, fgC}):xs) =
  pictures [ translate (fst fgP) (snd fgP) $ color fgC $ tfigure fgT
           , translate 0 0 $ color fgC $ text "Boo!"]
           --, translate 0 0 $ color ds $ rectangleSolid (-fromInteger (snd disp)/2) (fromInteger (fst disp)/2)]

  where tfigure Прямоугольник = rectangleSolid 15 15
        tfigure Круг = circleSolid 10


render OnlyFigure _ lf =
  pictures $ fmap
    (\(Figure {fgP,fgT,fgC}) ->
       translate (fst fgP) (snd fgP) $ color fgC $ tfigure fgT)
    lf

  where tfigure Прямоугольник = rectangleSolid 15 15
        tfigure Круг = circleSolid 10


render FigureImageAndStaticWheel_OrStaticWheel _ lf =
  pictures $ fc_ft $ fmap
    (\(Figure {fgP, fgT, fgC, fgWhs}) ->
       (fcps fgWhs, translate (fst fgP) (snd fgP) $ color fgC $ tfigure fgT))
    lf

  where fc_ft ((cs,f):xs) = cs++[f]++fc_ft xs
        fc_ft [] = []

        fcps (Wheel {whT,whC,whP2}:xs) =
          (translate (fst whP2) (snd whP2) $ color whC $ tfigure whT):fcps xs
        fcps [] = []

        tfigure Прямоугольник = rectangleSolid 15 15
        tfigure Круг = circleSolid 10


render FigureAndWheel _ lf =
  pictures $ whs_f $ fmap
    (\(Figure {fgP, fgT, fgC, fgWhs}) ->
       (whs fgWhs, translate (fst fgP) (snd fgP) $ color fgC $ tfigure fgT))
    lf

  where whs_f ((cs,f):xs) = cs++[f]++whs_f xs
        whs_f [] = []

        whs (Wheel {whDg,whT,whC,whP2}:xs) =
          (translate (fst whP2) (snd whP2) $ rotate whDg $ color whC $ rectangleSolid 10 5):whs xs
        whs [] = []

        tfigure Прямоугольник = rectangleSolid 15 15
        tfigure Круг = circleSolid 10


render FigureAndWheelAndVectors (Just angle) lf =
  pictures $ whs_f $ fmap
    (\(Figure {fgP, fgDg, fgT, fgC, fgWhs}) ->
       (whs fgDg fgWhs, translate (fst fgP) (snd fgP) $ color fgC $ tfigure fgT))
    lf

  where whs_f ((cs,f):xs) = cs++[f]++whs_f xs
        whs_f [] = []
        whs dg (Wheel {whDg, whC, whP2}:xs) =
          let (degreederx,degreedery) = whP2 ^+^ (mulSV 10 $ unitVectorAtAngle $ degToRad (whDg+dg))
          in
          (translate (fst whP2) (snd whP2) $ rotate (-(whDg+dg)) $ color whC $ rectangleSolid 20 5):
          --(translate actcpx actcpy $ color ds $ circleSolid 3):
          (translate degreederx degreedery $ color white $ circleSolid 2):whs dg xs
        whs _ [] = []

        tfigure Прямоугольник = rectangleSolid 15 15
        tfigure Круг = circleSolid 10


{-
render OnlyFigure lf =
  pictures $ justPairs lf

  where tfigure Прямоугольник = rectangleSolid 15 15
        tfigure Круг = circleSolid 10

        justPairs (fgF:fgS:xs) =
          let dgn p dg lenn = p ^+^ (mulSV lenn $ unitVectorAtAngle $ degToRad dg)
              dgN fN fFVvsSV lenn = dgn (fgP fN) (fFVvsSV) lenn

              dgFVtoSV = radToDeg $ argV $ normalcyVStoVF (fgP fgF) (fgP fgS)
              dgSVtoFV = radToDeg $ argV $ normalcyVStoVF (fgP fgS) (fgP fgF)
              lenV dgn = range (9,9) (piDegree dgn)
              console p = show $ fromEnum p
{-              angleV = angleDecimal (15,15)
              (fposFV,sposFV) = (angleV ^* lenFV) %+% (fgP fgF)
              (fposSV,sposSV) = (angleV ^* lenSV) %+% (fgP fgS)
-}
              translr fgN fdgN =
                let (lposN,rposN) = fgP fgN
                in
                if (fgT fgN) == Прямоугольник
                then
                  [(translate lposN rposN $ color (fgC fgN) $ tfigure (fgT fgN))
                  ,(translate lposN (rposN-35) $ color dr $ scale 0.1 0.1 $ text (console lposN))
                  ,(translate lposN (rposN-55) $ color dr $ scale 0.1 0.1 $ text (console rposN))
                  ,(translate (fst fdgN) (snd fdgN) $ color black $ circleSolid 2)]
                else
                  [(translate lposN rposN $ color (fgC fgN) $ tfigure (fgT fgN))
                  ,(translate lposN (rposN-35) $ color dr $ scale 0.1 0.1 $ text (console lposN))
                  ,(translate lposN (rposN-55) $ color dr $ scale 0.1 0.1 $ text (console rposN))
                  ,(translate (fst fdgN) (snd fdgN) $ color black $ circleSolid 2)]

              translrF = translr fgF (dgN fgF dgSVtoFV (lenV dgFVtoSV))
              translrS = translr fgS (dgN fgS dgFVtoSV (lenV dgSVtoFV))
          in
          translrF ++ translrS ++ justPairs xs

        justPairs (Figure {fgP, fgT, fgC}:xs) =
          (translate (fst fgP) (snd fgP) $ color fgC $ tfigure fgT):
          justPairs xs
        justPairs [] = []
-}
