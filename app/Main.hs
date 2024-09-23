import Prelude
import Control.Monad
import Data.Maybe
import Text.Read (readMaybe)
import Graphics.Gloss.Interface.Environment (getScreenSize)

import Game
import Lib


l'я = "\nLanguage/Язык [en/ru/ру]: "

ru =
  ["\nДобро пожаловать в игру!\nВыберите один из 9 режимов:\n"
  ,"(1) Управление шариком \n(2) Множество \n(3) Изображение вложенности \n(4) Логика вложенностей \n(5) Вращение шариков вокруг центральной фигуры \n(6) Изображение колёс \n(7) Вращение колёс вокруг центральной фигуры и своей оси, \n    с изображением векторов \n"
  ,"\nВведите следующее:\n"
  ,"1. Число составных \n2. Число вложенных \n3. Делитель экрана, для области вложенных \n4. Градус поворота\n"]

en =
  ["\nWelcome to the game!\nSelect one of the 9 modes:\n"
  ,"(1) Ball control \n(2) Array \n(3) Nesting image \n(4) Subdivision \n(5) Rotating the balls around the central figure \n(6) Wheel image \n(7) Rotation of the wheels around the central figure and its axis, \n    with vector representation \n"
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
      init = (Just numer, Just enclosure, Just comrat, Just angle)

  case gch of
    "1" -> game Ball'Control g (Nothing, Nothing, Nothing, Nothing)
    "2" -> game Array g (Just numer, Nothing, Nothing, Just angle)
    "3" -> game Nesting g init
    "4" -> game Substructure g init
    "5" -> game Rotation'Balls g init
    "6" -> game Wheels g init
    "7" -> game Rotation'Wheels g init
    _ -> undefined
