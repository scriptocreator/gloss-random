--(Figure,pos,deg,spd,typ,col,whs,pos2,(*%),(%*),scene,disp,dr,ds,window,background,fps,(^*),(^+^),state)
module Lib where

import Prelude
import Graphics.Gloss


type Position = (Float, Float)
type Speed = (Float, Float)
data Type = Прямоугольник | Круг
type Figures = [Figure]
type Degree = Float

data Figure
  = Figure {fgP :: Position, fgDg :: Degree, fgS :: Speed, fgT :: Type, fgC :: Color, fgWhs :: [Figure]}
  | Wheel {whP :: Position, whDg :: Degree, whT :: Type, whC :: Color, whP2 :: Position}
  | FigureVoid
data Geometry = Angle Degree Geometry Geometry | Parallel [Geometry] | Linear [Geometry] | Line (Float,Float) deriving Show
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

(aa,ab) *% (f,b) = (f aa b,ab)
(aa,ab) %* (f,b) = (aa,f ab b)

scene = mixColors 50 50 green orange

disp :: Num a => (a, a)
disp = (900, 1600)
dr = dark red
ds = dark scene

window :: Display
window = FullScreen

background :: Color
background = scene

fps :: Int
fps = 60

(^*) :: Num a => (a,a) -> a -> (a,a)
(^*) (a,b) p = (a*p,b*p)
(^+^) :: Num a => (a,a) -> (a,a) -> (a,a)
(^+^) (aa,ab) (ba,bb) = (aa+ba,ab+bb)

state :: Float -> Int -> Bool
state sn scn =
  sn >=  toEnum scn/2 ||
  sn <= -toEnum scn/2
