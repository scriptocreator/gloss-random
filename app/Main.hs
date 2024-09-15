import Prelude
import Control.Monad
import Data.Maybe
import Text.Read (readMaybe)
import Graphics.Gloss.Interface.Environment (getScreenSize)

import Ball
import Screen
import Array
import Nesting
import Substructure
import Rotation'Balls
import Wheel
import Rotation'Wheels


l'я = "\nLanguage/Язык [en/ru/ру]: "

ru =
  ["\nДобро пожаловать в игру!\nВыберите один из 9 режимов:\n"
  ,"(1) Мяч \n(2) Экран \n(3) Множество \n(4) Изображение вложенности \n(5) Логика вложенностей \n(6) Вращение шариков вокруг центральной фигуры \n(7) Изображение колёс \n(8) Вращение колёс вокруг центральной фигуры и своей оси, \n    с изображением векторов \n"
  ,"\nВведите следующее:\n"
  ,"1. Число составных \n2. Число вложенных \n3. Делитель экрана, для области вложенных \n4. Градус поворота\n"]

en =
  ["\nWelcome to the game!\nSelect one of the 9 modes:\n"
  ,"(1) Ball \n(2) Screen \n(3) Array \n(4) Nesting image \n(5) Subdivision \n(6) Rotating the balls around the central figure \n(7) Wheel image \n(8) Rotation of the wheels around the central figure and its axis, \n    with vector representation \n"
  ,"\nEnter the following:\n"
  ,"1. Number of composite \n2. Number of nested \n3. Screen divider, for nested \n4. Degree of turning\n"]

main :: IO ()
main = do
  g <- getScreenSize

  putStrLn l'я
  rl'я <- getLine
  let pl'я =
        case rl'я of
          "en" -> en
          "ru" -> ru
          "ру" -> ru
          _ -> en

  putStrLn (pl'я !! 0)
  putStrLn (pl'я !! 1)

  gch <- getLine
  putStrLn (pl'я !! 2)
  putStrLn (pl'я !! 3)
  [put, compon, ratio, num] <- replicateM 4 getLine

  let readput a b =
        let mayber = fmap (*1) (readMaybe a)
        in
        if isJust mayber
        then fromJust mayber
        else b

      numer = readput put 10
      enclosure = readput compon 3
      rater = readput ratio 15
      angle = readput num 10

      comrat = (div (fst g) rater, div (snd g) rater)
      init = (g, numer, enclosure, comrat, angle)

  case gch of
    "1" -> ball g
    "2" -> screen g
    "3" -> array g numer
    "4" -> nesting init
    "5" -> substructure init
    "6" -> rotation'balls init
    "7" -> wheel init
    "8" -> rotation'wheels init
    _ -> undefined
