import Data.Functor ( (<&>) )
import Data.List.Utils (countElem)
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  (left, right) <- parseFile
  print $ similarity right left

similarity :: [Int] -> [Int] -> Int
similarity r = sum . map score
  where
    score x = x * countElem x r

parseFile :: IO ([Int], [Int])
parseFile = getArgs >>= readFile . head <&> parse

parse :: String -> ([Int], [Int])
parse = unzip . map parseLine . lines

parseLine :: String -> (Int, Int)
parseLine line =
  let [x, y] = map read (words line)
   in (x, y)
