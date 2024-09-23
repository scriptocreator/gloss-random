{-# LANGUAGE NamedFieldPuns #-}

module PositAndSpeed where

import Lib
import Prelude
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle


map_position :: PatternMatching
             -> Either Float (Float,Float)
             -> Figure
             -> Figure


map_position OnlyFigure (Left s) (figure@(Figure {fgP,fgS})) =
  figure
  `pos` (px', py')

  where px' = fst fgP + fst fgS * s
        py' = snd fgP + snd fgS * s


map_position FigureAndStaticWheel (Left s) (figure@(Figure {fgP,fgS,fgWhs})) =
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


map_position FigureAndWheel (Right (x,y)) (wheel@(Wheel {whP})) =
  wheel
  `pos2` (fst whP+x,snd whP+y)


map_position FigureAndWheel (Left s) (figure@(Figure {fgP, fgS, fgWhs})) =
  figure
  `pos` (px', py')
  `whs`
    (( map_position FigureAndWheel (Right (px',py')) )
     `fmap`
     fgWhs)

  where px' = fst fgP + fst fgS * s
        py' = snd fgP + snd fgS * s



map_collision :: PatternMatching
              -> (Int,Int)
              -> Figure
              -> Figure


map_collision OnlyFigure (dx,dy) (figure@(Figure {fgP,fgDg,fgS})) =
  figure
  `deg` updg
  `spd` (upsx,upsy)

  where sc' st s =
          if st
          then (True,-s)
          else (False,s)

        (logx,upsx) = sc' (state (fst fgP) dx) (fst fgS)
        (logy,upsy) = sc' (state (snd fgP) dy) (snd fgS)

        updg =
          if logx || logy
          then radToDeg $ argV (upsx,upsy)
          else fgDg
--lengthVV (fgP fgF) (fgP fgS) > (lengthV (fgP fgF) + lengthV (fgP fgS))


map_collision FigureAndWheel_OrStaticWheel (dx,dy) (figure@(Figure {fgP, fgDg, fgS, fgWhs})) =
  figure
  `deg` updg
  `spd` (upsx,upsy)

  where fp_fs st bc s =
          if st || bc
          then (True,-s)
          else (False,s)

        (logx,upsx) = fp_fs (state (fst fgP) dx) (find_collision True dx fgWhs) (fst fgS)
        (logy,upsy) = fp_fs (state (snd fgP) dy) (find_collision False dy fgWhs) (snd fgS)

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
