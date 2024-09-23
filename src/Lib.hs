module Lib where

import Prelude
import Graphics.Gloss
import Graphics.Gloss.Data.Vector


type Position = (Float, Float)
type Speed = (Float, Float)
data Type = Прямоугольник | Круг deriving (Show, Eq)
type Figures = [Figure]
type Degree = Float

data Figure
  = Figure {fgP :: Position, fgDg :: Degree, fgS :: Speed, fgT :: Type, fgC :: Color, fgWhs :: [Figure]}
  | Wheel {whP :: Position, whDg :: Degree, whT :: Type, whC :: Color, whP2 :: Position}
  | FigureVoid

data PatternMatching
  = OneFigure
  | OnlyFigure
  | FigureImageAndStaticWheel
  | FigureImageAndStaticWheel_OrStaticWheel
  | FigureAndWheel_OrStaticWheel
  | FigureAndStaticWheel
  | FigureAndWheel
  | FigureAndWheelAndVectors deriving (Show, Read)

data Minigames
  = Ball'Control
  | Array
  | Nesting
  | Substructure
  | Rotation'Balls
  | Wheels
  | Rotation'Wheels deriving (Show, Read)

{-
data Geometry
  = Angle Degree Geometry Geometry
  | Parallel [Geometry]
  | Linear [Geometry]
  | Line (Float,Float) deriving Show
-}
--data Fi_Wh = P | D | S | T | С | Whs


(fg@(Figure{})) `pos` upd = fg {fgP = upd}
(wh@(Wheel{})) `pos` upd = wh {whP = upd}

(fg@(Figure{})) `deg` upd = fg {fgDg = upd}
(wh@(Wheel{})) `deg` upd = wh {whDg = upd}

(fg@(Figure{})) `spd` upd = fg {fgS = upd}
(wh@(Wheel{})) `spd` _ = wh

(fg@(Figure{})) `typ` upd = fg {fgT = upd}
(wh@(Wheel{})) `typ` upd = wh {whT = upd}

(fg@(Figure{})) `col` upd = fg {fgC = upd}
(wh@(Wheel{})) `col` upd = wh {whC = upd}

(fg@(Figure{})) `whs` upd = fg {fgWhs = upd}
(wh@(Wheel{})) `whs` _ =  wh

(fg@(Figure{})) `pos2` _ = fg
(wh@(Wheel{})) `pos2` upd = wh {whP2 = upd}

(aa,ab) +% b = (aa+b,ab)
(aa,ab) -% b = (aa-b,ab)
(aa,ab) %+ b = (aa,ab+b)
(aa,ab) %- b = (aa,ab-b)
(aa,ab) %+% (ba,bb) = (aa+ba,ab+bb)
(aa,ab) %-% (ba,bb) = (aa-ba,ab-bb)

scene =
  mixColors 50 50 green orange

--disp :: Num a => (a, a)
--disp = (900, 1600)
dr = dark red
ds = dark scene

window :: Display
window = FullScreen

background :: Color
background = scene

fps :: Int
fps = 60

(*^) :: Num a => (a,a) -> a -> (a,a) 
(^+) :: Num a => (a,a) -> a -> (a,a)
(^+^) :: Num a => (a,a) -> (a,a) -> (a,a)


(*^) (a,b) p = (a*p,b*p)
tup ^* scl = mulSV scl tup
(^+) (a,b) p = (a+p,b+p)
(^+^) (aa,ab) (ba,bb) = (aa+ba,ab+bb)


state :: Float -> Int -> Bool
state sn scn =
  sn >=  toEnum scn/2 ||
  sn <= -toEnum scn/2

rangeL n xer =
  take n xer:
  rangeL n (drop n xer)

rangeL _ [] = []

{-|
piHalf = pi / 2

range (10,50) 0
> 10.0
range (10,50) piHalf
> 50.0
range (10,50) pi
> 10.0
range (10,50) (pi+piHalf)
> 50.0
range (10,50) (pi*2)
> 10.0

piDegree n = ((pi * 2) / 360) * n
|-}
range (a,b) dg =
  min ((a/) $ abs $ cos dg)
      ((b/) $ abs $ sin dg)

piDegree n =
  ((pi * 2) / 360) * n

lengthV (a,b) =
  sqrt ((a ** 2) + (b ** 2))

normalcyVStoVF a b = a %-% b

angleDecimal (a,b) =
  let a2 = a * 2
      b2 = b * 2
  in
  if a2 >= b2
  then (a2 / b2, 1)
  else (1, b2 / a2)
{-
pairTuple (x:y:xs) = (Just x,Just y):pairTuple xs
pairTuple (x:xs) = (Just x,Nothing):pairTuple xs
pairTuple [] = []
-}
