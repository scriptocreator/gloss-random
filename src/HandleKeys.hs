{-# LANGUAGE NamedFieldPuns #-}

module HandleKeys where

import Lib
import Prelude
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle


handleKeys :: PatternMatching
           -> Float
           -> (Int,Int)
           -> Event
           -> Figures
           -> Figures


handleKeys OneFigure _ _ (EventKey (Char 'a') Up _ _) (figure:xs) =
  let upDg = fgDg figure + 45
      upSpd = mulSV 100 $ unitVectorAtAngle $ degToRad upDg
  in
  [figure
   `deg` upDg
   `spd` upSpd]

handleKeys OneFigure _ _ (EventKey (Char 'd') Up _ _) (figure:xs) =
  let upDg = fgDg figure - 45
      upSpd = mulSV 100 $ unitVectorAtAngle $ degToRad upDg
  in
  [figure
   `deg` upDg
   `spd` upSpd]

handleKeys OneFigure _ _ (EventKey (Char 'w') Up _ _) (figure:xs) =
  let upDg = fgDg figure + 135
      upSpd = mulSV 100 $ unitVectorAtAngle $ degToRad upDg
  in
  [figure
   `deg` upDg
   `spd` upSpd]

handleKeys OneFigure _ _ (EventKey (Char 's') Up _ _) (figure:xs) =
  let upDg = fgDg figure - 135
      upSpd = mulSV 100 $ unitVectorAtAngle $ degToRad upDg
  in
  [figure
   `deg` upDg
   `spd` upSpd]

handleKeys OneFigure _ _ (EventKey (Char '/') Up _ _) (figure@(Figure {fgT=Прямоугольник}):xs) =
  [figure
   `typ` Круг]

handleKeys OneFigure _ _ (EventKey (Char '/') Up _ _) (figure@(Figure {fgT=Круг}):xs) =
  [figure
   `typ` Прямоугольник]

handleKeys OneFigure _ _ (EventKey (Char '0') Up _ _) (figure:xs) =
  let newDg = 45.0
      upSpd = mulSV 100 $ unitVectorAtAngle $ degToRad newDg
  in
  [figure
   `pos` (0, 0)
   `deg` newDg
   `spd` upSpd]

handleKeys OneFigure _ _ (EventKey (Char ec) Up _ _) (figure:xs)
  | ec == 'r' =
    [figure
     `col` red]

  | ec == 'g' =
    [figure
     `col` green]

  | ec == 'b' =
    [figure
     `col` blue]

  | ec == '-' =
    [figure
     `col` (dark $ fgC figure)]

  | ec == '=' =
    [figure
     `col` (light $ fgC figure)]


handleKeys OnlyFigure angle (scx,scy) (EventKey (Char '-') Up _ _) game =
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

handleKeys OnlyFigure angle (scx,scy) (EventKey (Char '=') Up _ _) game =
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


handleKeys FigureAndStaticWheel angle (scx,scy) (EventKey (Char '[') Up _ _) game =
  (\(figure@(Figure {fgP, fgWhs})) ->
     let rotateCache = rotateWhs (-angle) fgWhs
     in
     figure
     `whs`
       if fwhs_ifer (scx,scy) fgP rotateCache
       then fgWhs
       else
         (\(wheel, cachen) ->
            wheel
            `pos` cachen)
         `fmap`
         (zip fgWhs rotateCache) )
  `fmap`
  game

handleKeys FigureAndStaticWheel angle (scx,scy) (EventKey (Char ']') Up _ _) game =
  (\(figure@(Figure {fgP, fgWhs})) ->
     let rotateCache = rotateWhs angle fgWhs
     in
     figure
     `whs`
       if fwhs_ifer (scx,scy) fgP rotateCache
       then fgWhs
       else
         (\(wheel, cachen) ->
            wheel
            `pos` cachen)
         `fmap`
         (zip fgWhs rotateCache) )
  `fmap`
  game

handleKeys FigureAndStaticWheel _ _ (EventKey (Char '0') Up _ _) game =
  (\figure ->
     let updg = fgDg figure+180
         upsn = mulSV 100 $ unitVectorAtAngle $ degToRad updg
     in
     figure
     `deg` updg
     `spd` upsn)
  `fmap`
  game

handleKeys FigureAndStaticWheel angle scn (EventKey (Char '-') ea eb ec) game =
  handleKeys FigureAndWheel_OrStaticWheel angle scn (EventKey (Char '-') ea eb ec) game

handleKeys FigureAndStaticWheel angle scn (EventKey (Char '=') ea eb ec) game =
  handleKeys FigureAndWheel_OrStaticWheel angle scn (EventKey (Char '=') ea eb ec) game



handleKeys FigureAndWheel angle (scx,scy) (EventKey (Char '[') Up _ _) game =
  (\(figure@(Figure {fgP, fgWhs})) ->
     let rotateCache = rotateWhs (-angle) fgWhs
     in
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

handleKeys FigureAndWheel angle (scx,scy) (EventKey (Char ']') Up _ _) game =
  (\(figure@(Figure {fgP, fgWhs})) ->
     let rotateCache = rotateWhs angle fgWhs
     in
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

handleKeys FigureAndWheel angle scn (EventKey (Char '-') ea eb ec) game =
  handleKeys FigureAndWheel_OrStaticWheel angle scn (EventKey (Char '-') ea eb ec) game

handleKeys FigureAndWheel angle scn (EventKey (Char '=') ea eb ec) game =
  handleKeys FigureAndWheel_OrStaticWheel angle scn (EventKey (Char '=') ea eb ec) game



handleKeys FigureAndWheel_OrStaticWheel angle scn (EventKey (Char '-') Up _ _) game =
  (\figure ->
     let updg = fgDg figure-angle
         upsn = mulSV 100 $ unitVectorAtAngle $ degToRad updg
         rotateCache = rotateWhs angle (fgWhs figure)
     in
     if fwhs_ifer scn (fgP figure) rotateCache
     then
       figure
     else
       figure
       `deg` updg
       `spd` upsn)
  `fmap`
  game

handleKeys FigureAndWheel_OrStaticWheel angle scn (EventKey (Char '=') Up _ _) game =
  (\figure ->
     let updg = fgDg figure+angle
         upsn = mulSV 100 $ unitVectorAtAngle $ degToRad updg
         rotateCache = rotateWhs angle (fgWhs figure)
     in
     if fwhs_ifer scn (fgP figure) rotateCache
     then
       figure
     else
       figure
       `deg` updg
       `spd` upsn)
  `fmap`
  game

{-
handleKeys angle scn (EventKey (Char 'х') Up a b) figure =
  handleKeys angle scn (EventKey (Char '[') Up a b) figure

handleKeys angle scn (EventKey (Char 'ъ') Up a b) figure =
  handleKeys angle scn (EventKey (Char ']') Up a b) figure
-}
--handleKeys angle _ (EventKey (SpecialKey KeyTab) Up _ _) game = game
--handleKeys angle _ (EventKey (SpecialKey KeyCtrlL) Up _ _) game = game

handleKeys _ _ _ _ game = game


fwhs_ifer :: (Int,Int)
          -> (Float,Float)
          -> [(Float,Float)]
          -> Bool

fwhs_ifer (scx,scy) (x,y) ((xx,xy):xs) =
  if state (xx+x) scx || state (xy+y) scy
  then True
  else fwhs_ifer (scx,scy) (x,y) xs

fwhs_ifer _ _ [] = False


rotateWhs angle (wheel:xs) =
  rotateV (degToRad angle) (whP wheel):rotateWhs angle xs

rotateWhs _ [] = []
