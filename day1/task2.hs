import Data.List.Utils (countElem)
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  contents <- readFile filename
  let (left, right) = unzip . map parseLine $ lines contents
  print $ similarity right left

parseLine :: String -> (Int, Int)
parseLine line =
  let [x, y] = map read (words line)
   in (x, y)

similarity :: [Int] -> [Int] -> Int
similarity r = sum . map score
  where
    score x = x * countElem x r
