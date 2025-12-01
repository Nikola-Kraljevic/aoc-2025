module Day01 where

-- Coz the Either has the same constructors
import Data.Foldable
import Paths_aoc2025 (getDataFileName)
import Prelude hiding (Left, Right)

data Rotation = Left Int | Right Int deriving (Show, Eq)

parseRotation :: String -> Rotation
parseRotation [] = error "Empty rotation"
parseRotation (direction : ammount) = case direction of
  'L' -> Left $ read ammount
  'R' -> Right $ read ammount
  _ -> error "Unexpected direction"

rotate :: Int -> Rotation -> Int
rotate state (Right ammount) = (state + (ammount `mod` 100)) `mod` 100
rotate state (Left ammount) = ((state + 100) - (ammount `mod` 100)) `mod` 100

countZeroes :: (Int, Int) -> Rotation -> (Int, Int)
countZeroes (zeroesCount, state) rotation = case rotate state rotation of
    0 -> (zeroesCount + 1, 0)
    x -> (zeroesCount, x)


getPassword :: (Int,Int) -> [Rotation] -> (Int,Int)
getPassword = foldl' countZeroes

day01 :: IO ()
day01 = do
  inputLines <- lines <$> (getDataFileName "day01-input.txt" >>= readFile)
  let rotationList = fmap parseRotation inputLines
  print $ fst $ getPassword (0,50) rotationList
