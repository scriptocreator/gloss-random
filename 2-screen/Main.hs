module Main (main) where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Data.Picture
--import Graphics.UI.GLUT.Fonts as GLUTF
--import Graphics.UI.GLFW as GLFW
import System.Random
import System.Random.Stateful
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)


type Position = (Float, Float)
type Speed = (Float, Float)
data Type = Прямоугольник | Круг
type Figures = Figure

data Figure = Figure Position Speed Type Color


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

initialState :: (Int,Int) -> (Int,Int) -> Maybe Figure
initialState (rpx,rpy) (rsx,rsy) = Just $ Figure (toEnum rpx,toEnum rpy) (toEnum rpx,toEnum rsy) Круг (dark red)


render :: Maybe Figure -> Picture
render (Just (Figure (px,py) _ ff cdr)) =
  pictures [ translate px py $ color cdr $ figure ff
           , translate 0 0 $ color cdr $ text "Boo!"]
           --, translate 0 0 $ color ds $ rectangleSolid (-fromInteger (snd disp)/2) (fromInteger (fst disp)/2)]
   where
    
    figure Прямоугольник = rectangleSolid 10 10
    figure Круг = circleSolid 10


handleKeys :: Event -> Maybe Figure -> Maybe Figure
handleKeys (EventKey (Char 'a') _ _ _) (Just (Figure a (sx,sy) ff c)) = Just $ Figure a (sx-50,sy) ff c
handleKeys (EventKey (Char 'd') _ _ _) (Just (Figure a (sx,sy) ff c)) = Just $ Figure a (sx+50,sy) ff c
handleKeys (EventKey (Char 'w') _ _ _) (Just (Figure a (sx,sy) ff c)) = Just $ Figure a (sx,sy+50) ff c
handleKeys (EventKey (Char 's') _ _ _) (Just (Figure a (sx,sy) ff c)) = Just $ Figure a (sx,sy-50) ff c
handleKeys (EventKey (Char '/') Up _ _) (Just (Figure a sn Прямоугольник c)) = Just $ Figure a sn Круг c
handleKeys (EventKey (Char '/') Up _ _) (Just (Figure a sn Круг c)) = Just $ Figure a sn Прямоугольник c

handleKeys (EventKey (Char 'r') _ _ _) (Just (Figure a b ff _)) = Just $ Figure a b ff blue
handleKeys (EventKey (Char 'g') _ _ _) (Just (Figure a b ff _)) = Just $ Figure a b ff green
handleKeys (EventKey (Char 'b') _ _ _) (Just (Figure a b ff _)) = Just $ Figure a b ff blue
handleKeys (EventKey (Char '-') _ _ _) (Just (Figure a b ff c)) = Just $ Figure a b ff (dark c)
handleKeys (EventKey (Char '=') _ _ _) (Just (Figure a b ff c)) = Just $ Figure a b ff (light c)
handleKeys (EventKey (SpecialKey KeyBackspace) _ _ _) (Just (Figure a b ff _)) = Just $ Figure a b ff dr

handleKeys (EventKey (Char '0') _ _ _) (Just (Figure a b ff c)) = Just $ Figure a b ff c
handleKeys _ game = game


update :: (Int,Int) -> Float -> Maybe Figure -> Maybe Figure
update g s = collision g . position s

position :: Float -> Maybe Figure -> Maybe Figure
position s (Just (Figure (px,py) (sx,sy) ff c)) = Just $ Figure (px', py') (sx,sy) ff c
   where
    px' = px + sx * s
    py' = py + sy * s

collision :: (Int,Int) -> Maybe Figure -> Maybe Figure
collision (dx,dy) (Just (Figure (px,py) (sx,sy) ff c)) = Just $ Figure (px,py) (sxc', syc') ff c
   where
    sxc' = if state px dx then -sx else sx
    syc' = if state py dy then -sy else sy

state :: Float -> Int -> Bool
state sn d =
  (sn >=  toEnum d/2) ||
  (sn <= -toEnum d/2)


main :: IO ()
main = do
  g <- getScreenSize
  rp <- randomRIO ((div (-fst g) 2, div (-snd g) 2), (div (fst g) 2, div (snd g) 2))
  rs <- randomRIO ((div (-400) 2, div (-400) 2), (div 400 2, div 400 2))
  print g
  print rp
  print rs 
  play window background fps (initialState rp rs) render handleKeys (update g)
