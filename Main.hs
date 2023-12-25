import System.IO
import System.IO.Unsafe
import System.Random
import Data.List
import Control.Monad (sequence, replicateM)
import Text.Printf

-- Coord represents a set of possible values along one axis; there are only 3 unique of them
data Coord3 = A | B | C deriving (Eq, Enum, Show)

data Card = Card {x1 :: Coord3, x2 :: Coord3, x3 :: Coord3, x4 :: Coord3} deriving (Show, Eq)

isSet (c1, c2, c3)
  = let axesGetters = [x1, x2, x3, x4]
        allDifferOrMatch xi c1 c2 c3 = xi c1 == xi c2 && xi c2 == xi c3 || xi c1 /= xi c2 && xi c1 /= xi c3 && xi c2 /= xi c3
        in all (\xi -> allDifferOrMatch xi c1 c2 c3) axesGetters

genRandomCard :: IO Card
genRandomCard = do
  r <- replicateM 4 $ randomRIO (0,2::Int)
  let rs = map toEnum r
  return (Card (rs!!0) (rs!!1) (rs!!2) (rs!!3))

tuplify3 :: [a] -> (a,a,a)
tuplify3 [x,y,z] = (x,y,z)

triples xs = filter ((3==).length) $ subsequences xs

takeUniq n xs = head $ dropWhile (\l -> length l /= n) $ scanl (\l x -> if x `elem` l then l else x:l) [] xs

interweavingRepeatM :: IO a -> IO [a]
interweavingRepeatM action = unsafeInterleaveIO ((:) <$> action <*> interweavingRepeatM action)

experiment :: Int -> IO Bool
experiment n = do
  cards <- takeUniq n <$> interweavingRepeatM genRandomCard
  let sets = filter (isSet . tuplify3) $ triples cards
--  if sets /= [] then print $ head sets else pure ()
  return (sets /= [])

experiments :: Int -> Int -> IO Int
experiments nCards nExperiments = do
  exps <- replicateM nExperiments $ experiment nCards
  let sets = length $ filter id exps
  let ratio = fromIntegral sets / fromIntegral nExperiments :: Float
  printf "|%d|%d|%d|%f|\n" nCards nExperiments sets ratio
  return sets

main = do
  print "|N|total|sets|ratio|\n"
  let nExperiments = 500000
  mapM_ (\nCards -> experiments nCards nExperiments) [3..7]
  print "---"
