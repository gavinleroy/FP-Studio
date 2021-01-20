module Sudoku where
  -- ( solveFromString 
  -- ) where

import Data.List            (intersect, intersperse, transpose, (\\), minimumBy, foldl')
import Control.Monad        (fmap, replicateM)
import Control.Applicative  (Alternative, (<|>))
import Data.Function        (on)
import Text.Read            (readMaybe)
import Data.Map             (empty, insertWith, foldlWithKey', filterWithKey, elems)

data Cell
  = Fixed Int
  | Choices [Int]
instance Show Cell where
  show (Fixed i)    = show i
  show (Choices xs)  = show xs
instance Eq Cell where
  (Fixed x) == (Fixed y)     = x == y
  (Choices x) == (Choices y) = x == y
  _ == _                     = False

type Size     = (Int, Int)
type Row a    = [a]
type Matrix a = [Row a]
type Board    = (Matrix Cell, Size)

-- Utility Functions --

toCell :: [Int] -> Maybe Cell
toCell []  = Nothing
toCell [x] = Just $ Fixed x
toCell xs  = Just $ Choices xs

isFixed :: Cell -> Bool
isFixed (Fixed _) = True
isFixed _         = False

numChoices :: Cell -> Int
numChoices (Choices xs) = length xs
numChoices _            = 1

isUnique :: Eq a => [a] -> Bool
isUnique []     = True
isUnique (x:xs) = not (x `elem` xs) && isUnique xs

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] =  []
chunksOf n xs =  take n xs : chunksOf n (drop n xs)

rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Row a]
cols = transpose

blocks :: Size -> Matrix a -> [Row a]
blocks (m, n) = join . map cols . chunk
  where
    chunk :: [[a]] -> [[[[a]]]]
    chunk     = chunksOf n . map chunkrows

    chunkrows :: [a] -> [[a]]
    chunkrows = chunksOf m

    join :: [[[[a]]]] -> [[a]]
    join      = map concat . concat

isValid :: Board -> Bool
isValid (b, mxn) 
  =  all isUnique (rows b) 
  && all isUnique (cols b)
  && all isUnique (blocks mxn b)
  && null [() | (Choices _) <- (concat b)]

isDeadEnd :: Board -> Bool
isDeadEnd (b, size) 
  = (any checkRowFix $ rows b)
  || (any checkRowFix $ cols b)
  || (any checkRowFix $ blocks size b)
  || any checkRowChoice b
  where
    checkRowFix :: Row Cell -> Bool
    checkRowFix = not . isUnique . filter isFixed

    checkRowChoice :: Row Cell -> Bool
    checkRowChoice = not . null . filter ((<=0) . numChoices)

fixedpointM :: (Eq a, Monad m) => (a -> m a) -> a -> m a
fixedpointM f b 
  = f b >>= \nb -> 
    if nb == b then 
      return nb 
    else fixedpointM f nb  

-- Pruning Steps --

uniqueCells :: Row Cell -> [[Int]]
uniqueCells
  = elems
  . filterWithKey (\xs ys -> length xs == length ys)
  . foldlWithKey' (\acc k ns -> insertWith p ns [k] acc) empty
  . foldl' (\acc ~(i, (Choices ns))-> 
      foldl' (\acc' k -> 
        insertWith p k [i] acc') acc ns) empty
  . filter (not . isFixed . snd)
  . zip [1..]
  where p = (flip (++))

pruneCellByKnown :: [Int] -> Cell -> Maybe Cell
pruneCellByKnown ns (Choices xs) = toCell $ xs \\ ns
pruneCellByKnown _ x = return x

pruneRowByKnown :: Row Cell -> Maybe (Row Cell)
pruneRowByKnown cells = traverse (pruneCellByKnown knowncells) cells
  where knowncells = [n | Fixed n <- cells]

pruneRowByUnique :: Row Cell -> Maybe (Row Cell)
pruneRowByUnique cells = case uniqueChoices of
  [] -> return cells
  _  -> traverse pruneCellMore cells 
  where
    uniqueChoices = uniqueCells cells

    pruneCellMore :: Cell -> Maybe Cell
    pruneCellMore cell@(Fixed _) = return cell
    pruneCellMore cell@(Choices ns)
      | intersection `elem` uniqueChoices = toCell intersection
      | otherwise                         = return cell
      where intersection = intersect ns $ concat uniqueChoices

pruneRow :: Row Cell -> Maybe (Row Cell)
pruneRow cells = fixedpointM pruneRowByKnown cells >>= pruneRowByUnique

pruneBoard :: Board -> Maybe Board      
pruneBoard = fixedpointM prune
  where
    prune :: Board -> Maybe Board
    prune (board, size) 
      = traverse pruneRow board
      >>= fmap transpose . traverse pruneRow . transpose
      >>= fmap (blocks size) . traverse pruneRow . blocks size
      >>= return . (flip (,) size)

-- Functions for Solving the Sudoku --

nextChoices :: Board -> (Board, Board)
nextChoices (board, size@(m, n)) 
  = ( list2Board size $ replCell i fstCell cellList
    , list2Board size $ replCell i rstCell cellList)
  where
    (i, fstCell, rstCell) 
      = setCell . minimumBy (compare `on` (numChoices . snd))
      . filter (not . isFixed . snd) $ cellList
    cellList = zip [0..] . concat $ board

    list2Board :: Size -> [Cell] -> Board
    list2Board s@(m,  n) l = (chunksOf (m * n) l, s)

    replCell :: Int -> Cell -> [(Int, Cell)] -> [Cell]
    replCell i newCell ((p, c):cs) 
      | i == p    = newCell : map snd cs
      | otherwise = c : replCell i newCell cs 

    setCell :: (Int, Cell) -> (Int, Cell, Cell)
    setCell (i, (Choices [a, b])) = (i, Fixed a, Fixed b)
    setCell (i, (Choices (x:xs))) = (i, Fixed x, Choices xs)

solve :: Board -> Maybe Board
solve board 
  = pruneBoard board 
  >>= solve'
  where
    solve' :: Board -> Maybe Board
    solve' b
      | isDeadEnd b  = Nothing
      | isValid b    = Just b
      | otherwise    = 
        let (b1, b2) = nextChoices b
        in solve b1 <|> solve b2

solveFromString :: String -> Maybe String
solveFromString cs 
  = parseBoard cs
  >>= solve
  >>= return . showBoard
  
-- Parsing a Board --

parseCell :: Int -> String -> Maybe Cell
parseCell mx "." = return $ Choices [1..mx]
parseCell mx cs  
  = readIntMaybe cs
  >>= return . Fixed

parseCellRow :: Int -> String -> Maybe (Row Cell)
parseCellRow mxn = mapM (parseCell mxn) . words

parseBoard :: String -> Maybe Board
parseBoard cs = do
  let lns = lines cs
  let size = head lns
  let sboard = tail lns
  [m, n]  <- mapM readIntMaybe . words $ size
  brd     <- mapM (parseCellRow (m * n)) sboard
  return (brd, (m, n))

make3x3 :: String -> String
make3x3 
  = unlines 
  . ((:) "3 3") 
  . map (intersperse ' ') 
  . chunksOf 9

-- Printing a Board --

readInt :: String -> Int
readInt = read

readIntMaybe :: String -> Maybe Int
readIntMaybe = readMaybe

row2string :: Int -> [Cell] -> String
row2string m [] = "\n"
row2string m cs 
  = (unwords $ map show $ take m cs) 
  ++ "   " 
  ++ (row2string m $ drop m cs)

concatins :: Int -> [String] -> String
concatins n [] = []
concatins n ss 
  = (concat $ take n ss) 
  ++ "\n" 
  ++ (concatins n $ drop n ss)

showBoard :: Board -> String
showBoard (board, (m, n)) 
  = unlines 
  [show m ++ "x" ++ show n
  , (concatins n $ map (row2string m) board)]

