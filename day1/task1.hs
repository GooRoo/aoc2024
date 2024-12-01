import Data.List (sort)
import Data.Tuple.Extra (both)
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  contents <- readFile filename
  let (left, right) = both sort . unzip . map parseLine $ lines contents
  print $ sum $ zipWith distance left right

parseLine :: String -> (Int, Int)
parseLine line =
  let [x, y] = map read (words line)
   in (x, y)

distance :: Int -> Int -> Int
distance l r = abs $ l - r
