module Cubes where

import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import System.IO

solveReboot' :: FilePath -> IO ()
solveReboot' fp = do
  cubes <- parseRebootCubes fp
  -- let cubes' = filter isSmall cubes
  print $ totalVolume cubes

isSmall :: Cube -> Bool
isSmall (Cube x1 x2 y1 y2 z1 z2 _) =
  x1 >= -50 && x2 <= 50 &&
  y1 >= -50 && y2 <= 50 &&
  z1 >= -50 && z2 <= 50

data Cube = Cube
  { xMin :: Int
  , xMax :: Int
  , yMin :: Int
  , yMax :: Int
  , zMin :: Int
  , zMax :: Int
  , isOn :: Bool
  } deriving (Show)

parseRebootCubes :: FilePath -> IO [Cube]
parseRebootCubes fp = (map parseCube . lines) <$> readFile fp

parseCube :: String -> Cube
parseCube input = case take 3 input of
  "on " -> (mkCube True) (parseCube (drop 3 input))
  "off" -> (mkCube False) (parseCube (drop 4 input))
  _ -> error $ "Invalid input " ++ input
  where
    parseCube cubeLine =
      let [xs, ys, zs] = splitOn "," cubeLine
          [x1, x2] = splitOn ".." (drop 2 xs)
          [y1, y2] = splitOn ".." (drop 2 ys)
          [z1, z2] = splitOn ".." (drop 2 zs)
      in  read <$> [x1, x2, y1, y2, z1, z2]

    mkCube :: Bool -> [Int] -> Cube
    mkCube on [x1, x2, y1, y2, z1, z2] = Cube
      x1 x2 y1 y2 z1 z2 on

cubeIntersection :: Cube -> Cube -> Maybe Cube
cubeIntersection
  c1@(Cube mnx1 mxx1 mny1 mxy1 mnz1 mxz1 _)
  c2@(Cube mnx2 mxx2 mny2 mxy2 mnz2 mxz2 on2) = if isValid newCube then Just newCube else Nothing
  where
    newMnx = max mnx1 mnx2
    newMxx = min mxx1 mxx2
    newMny = max mny1 mny2
    newMxy = min mxy1 mxy2
    newMnz = max mnz1 mnz2
    newMxz = min mxz1 mxz2
    newCube = Cube newMnx newMxx newMny newMxy newMnz newMxz on2

isValid :: Cube -> Bool
isValid (Cube mnx mxx mny mxy mnz mxz _) =
  mnx <= mxx && mny <= mxy && mnz <= mxz

basicVolume :: Cube -> Integer
basicVolume (Cube mnx mxx mny mxy mnz mxz _) = fromIntegral
  ( (mxx - mnx + 1)
  * (mxy - mny + 1)
  * (mxz - mnz + 1)
  )

correctedVolume :: Cube -> [Cube] -> Integer
correctedVolume cube otherCubes = base - (subvolumes intersections 0)
  where
    base = basicVolume cube
    intersections = catMaybes (map (cubeIntersection cube) otherCubes)

    subvolumes :: [Cube] -> Integer -> Integer
    subvolumes [] accum = accum
    subvolumes [onlyCube] accum = basicVolume onlyCube + accum
    subvolumes (firstCube : restCubes) accum = subvolumes restCubes (accum + correctedVolume firstCube restCubes)

totalVolume :: [Cube] -> Integer
totalVolume cubes = finalSum
  where
    litCubes = filter (\(_, c) -> isOn c) (zip [0,1..] cubes)
    finalSum = sum $ map (\(i, c) -> correctedVolume c (drop (i + 1) cubes) ) litCubes
