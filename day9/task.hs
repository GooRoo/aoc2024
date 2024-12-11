import Data.Char (digitToInt)
import Data.Functor ((<&>))
-- import Data.Tuple.Extra (both)
import System.Environment (getArgs)
import System.IO

data Chunk = File Int | Free Int deriving (Show)

type FileID = Int

data Block = Block Int | Empty deriving (Show, Eq)

main :: IO ()
main = do
  (files, frees) <- parseFile
  let d = disk files frees
  print . crc $ packedDisk d

parseFile :: IO ([Chunk], [Chunk])
parseFile = getArgs >>= readFile . head <&> parse

parse :: String -> ([Chunk], [Chunk])
parse = split . map digitToInt . head . lines

split :: [Int] -> ([Chunk], [Chunk])
split [] = ([], [])
split [x] = ([File x], [Free 0])
split (x1 : x2 : xs) = (File x1 : x1s, Free x2 : x2s)
  where
    (x1s, x2s) = split xs

disk :: [Chunk] -> [Chunk] -> [Block]
disk files = concat . zipWith repr (zip [0 ..] files)
  where
    repr :: (Int, Chunk) -> Chunk -> [Block]
    repr (i, File f) (Free x) = replicate f (Block i) ++ replicate x Empty

packedDisk :: [Block] -> [Block]
packedDisk d = take (length fs) $ pack d fs
  where
    fs = reverse . filter (/= Empty) $ d

    pack :: [Block] -> [Block] -> [Block]
    pack [] [] = []
    pack d [] = d
    pack [] fs = fs
    pack (Empty : ds) (Block f : fs) = Block f : pack ds fs
    pack (d : ds) fs = d : pack ds fs

packedDisk' :: [Block] -> [Block]
packedDisk' [] = []

crc :: [Block] -> Int
crc xs = sum $ zipWith (*) [0 ..] (map unwrap xs)
  where
    unwrap (Block x) = x
    unwrap Empty = 0
