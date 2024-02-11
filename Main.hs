import System.IO
import System.IO.Unsafe
import System.Random
import Data.List
import Control.Monad (sequence, replicateM)
import Text.Printf
import System.Random.Shuffle (shuffle')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (fromListWith, toList)
import Debug.Trace

-- Coord represents a set of possible values along one axis; there are only 3 unique of them
data Coord3 = A | B | C deriving (Eq, Enum, Show, Ord)

data Card = Card {x1 :: Coord3, x2 :: Coord3, x3 :: Coord3, x4 :: Coord3} deriving (Show, Eq, Ord)

toCard [x1, x2, x3, x4] = Card x1 x2 x3 x4

isSet (c1, c2, c3)
  = let axesGetters = [x1, x2, x3, x4]
        allDifferOrMatch xi c1 c2 c3 = xi c1 == xi c2 && xi c2 == xi c3 || xi c1 /= xi c2 && xi c1 /= xi c3 && xi c2 /= xi c3
        in all (\xi -> allDifferOrMatch xi c1 c2 c3) axesGetters

genRandomCard :: IO Card
genRandomCard = do
  r <- replicateM 4 $ randomRIO (0,2::Int)
  let rs = map toEnum r
  return (toCard rs)

tuplify3 :: [a] -> (a,a,a)
tuplify3 [x,y,z] = (x,y,z)

pairs xs = filter ((2==).length) $ subsequences xs
triples xs = filter ((3==).length) $ subsequences xs

uniq = foldl(\l x -> if x `elem` l then l else x:l) []

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

calcSetsFromRandomSelection = do
  print "|N|total|sets|ratio|\n"
  let nExperiments = 500000
  mapM_ (\nCards -> experiments nCards nExperiments) [3..7]
  print "---"

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (a:as) = Just a

findSet :: Card -> Set Card -> Maybe [Card]
findSet nc cs = safeHead $ filter (\[c1, c2] -> isSet (nc, c1, c2)) $ pairs $ Set.toList cs

addCardIfNoSet :: Card -> Maybe [Card] -> Set Card -> Set Card
addCardIfNoSet c mp s = case mp of
                         Just [c1, c2] ->
--                           trace ("Skipping card, because it forms a set: " ++ [c, c1, c2] ++ "; table has size " ++ (show . length $ s)) $ 
                             s
                         Nothing -> 
--                           trace ("Adding card to table of size " ++ (show . length $ s)) $ 
                           Set.insert c s

getMaximalSystem = do
  gen <- initStdGen
  let deck = shuffle' [toCard . map toEnum $ [x1, x2, x3, x4] | x1 <- [0..2], x2 <- [0..2], x3 <- [0..2], x4 <- [0..2]] (3^4) gen
  return $ foldr (\c table -> addCardIfNoSet c (findSet c table) table) Set.empty deck

main = do 
--  calcSetsFromRandomSelection
  expers <- replicateM 1500 getMaximalSystem
  let frequency xs = fromListWith (+) [(length x, 1) | x <- xs]
  print . frequency $ expers
  print $ filter (\xs -> length xs > 18) $ uniq expers
