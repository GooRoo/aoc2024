import Data.List (sort)
import Data.Functor ( (<&>) )
import Data.Tuple.Extra (both)
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  (left, right) <- both sort <$> parseFile
  print $ sum $ zipWith distance left right

distance :: Int -> Int -> Int
distance l r = abs $ l - r

parseFile :: IO ([Int], [Int])
parseFile = getArgs >>= readFile . head <&> parse

parse :: String -> ([Int], [Int])
parse = unzip . map parseLine . lines

parseLine :: String -> (Int, Int)
parseLine line =
  let [x, y] = map read (words line)
   in (x, y)
