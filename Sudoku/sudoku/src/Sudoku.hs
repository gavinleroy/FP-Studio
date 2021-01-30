{- 
 - Gavin Gray, University of Utah SP21
 - Sudoku Solver FP-Studio CS 6963
 -
 - This implementation expects a Sudoku board
 - to ONLY contain integers. This could however
 - be abstracted for a Cell to contain a 
 - (Set String), in which case letters
 - could also be used for the puzzle. 
 - -}

module Sudoku 
  -- ( solveFromString , makeString3x3)
  where
  
import Control.Applicative        ((<|>))
import Data.Function              (on)
import Data.IntSet                (IntSet)
-- import Data.Vector                (Vector)
import System.Random              (getStdRandom, randomR)
import Text.Read                  (readMaybe)

import qualified Data.List        as List
import qualified Data.Map         as Map
import qualified Data.IntSet      as Set
-- import qualified Data.Vector      as Vec

-- Data Definitions --

data Cell
  = Fixed Int
  | Choices IntSet
instance Show Cell where
  show (Fixed i)     = show i
  show (Choices _)   = "."
  -- show (Choices xs)  = show xs
instance Eq Cell where
  (Fixed x) == (Fixed y)     = x == y
  (Choices x) == (Choices y) = x == y
  _ == _                     = False

type Size     = (Int, Int)
type Row a    = [a]
type Matrix a = [Row a]
type Board    = (Matrix Cell, Size)

-- Utility Functions --

toCell :: IntSet -> Maybe Cell
toCell s 
  | ssize == 0 = Nothing
  | ssize == 1 = Just $ Fixed $ Set.findMin s
  | otherwise  = Just $ Choices s 
  where ssize  = Set.size s

isFixed :: Cell -> Bool
isFixed (Fixed _) = True
isFixed _         = False

numChoices :: Cell -> Int
numChoices (Choices xs) = Set.size xs
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
cols = List.transpose

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
  -- Make sure: No Choices left
  && null [() | (Choices _) <- (concat b)]

isDeadEnd :: Board -> Bool
isDeadEnd (b, size) 
  -- Unique Constraint
  = (any checkRowFix $ rows b)
  || (any checkRowFix $ cols b)
  || (any checkRowFix $ blocks size b)
  -- Remaining Possibilities Constraint
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
  = Map.elems
  . Map.filterWithKey (\xs ys -> length xs == length ys)
  . Map.foldlWithKey (\acc k ns -> Map.insertWith (++) ns [k] acc) Map.empty
  . List.foldl (\acc ~(i, (Choices setns))-> 
      Set.foldl (\acc' k ->
        Map.insertWith (++) k [i] acc') acc setns) Map.empty
  . filter (not . isFixed . snd)
  . zip [1..]

pruneRowByKnown :: Row Cell -> Maybe (Row Cell)
pruneRowByKnown cells = mapM pruneCellByKnown cells
  where 
    knowncells = Set.fromList [n | Fixed n <- cells]

    pruneCellByKnown :: Cell -> Maybe Cell
    pruneCellByKnown (Choices xs) = toCell $ Set.difference xs knowncells
    pruneCellByKnown x            = return x

pruneRowByUnique :: Row Cell -> Maybe (Row Cell)
pruneRowByUnique cells = case uniqueChoices of
  [] -> return cells
  _  -> mapM pruneCellByUnique cells 
  where
    uniqueChoices = map Set.fromList $ uniqueCells cells
    choiceSet = Set.unions uniqueChoices

    pruneCellByUnique :: Cell -> Maybe Cell
    pruneCellByUnique cell@(Fixed _) = return cell
    pruneCellByUnique cell@(Choices ns)
      | intersection `elem` uniqueChoices = toCell intersection
      | otherwise                         = return cell
      where intersection = Set.intersection ns choiceSet

pruneRow :: Row Cell -> Maybe (Row Cell)
pruneRow cells
  = fixedpointM pruneRowByKnown cells 
  >>= pruneRowByUnique

pruneBoard :: Board -> Maybe Board      
pruneBoard = fixedpointM prune
  where
    prune :: Board -> Maybe Board
    prune (board, size) 
      = mapM pruneRow board
      >>= fmap cols . mapM pruneRow . cols
      >>= fmap (blocks size) . mapM pruneRow . blocks size
      >>= return . flip (,) size -- (\x -> (x, size))

-- Functions for Solving the Sudoku --

nextChoices :: Board -> (Board, Board)
nextChoices (board, size@(m, n))
  = ( list2Board $ replCell i fstCell indexedCells
    , list2Board $ replCell i rstCell indexedCells )
  where
    (i, fstCell, rstCell) 
      = fixCell . List.minimumBy (compare `on` (numChoices . snd))
      . filter (not . isFixed . snd) $ indexedCells

    indexedCells :: [(Int, Cell)]
    indexedCells = zip [0..] . concat $ board

    list2Board :: [Cell] -> Board
    list2Board l = (chunksOf (m * n) l, size)

    replCell :: Int -> Cell -> [(Int, Cell)] -> [Cell]
    replCell i newCell ((p, c) : cs) 
      | i == p    = newCell : map snd cs
      | otherwise = c : replCell i newCell cs 

    fixCell :: (Int, Cell) -> (Int, Cell, Cell)
    -- fixCell (_, (Fixed _))   = error "unreachable fixCell pattern"
    fixCell (i, (Choices s))
      -- | Set.size s < 2       = error "unreachable fixCell pattern"
      | Set.size s == 2      = (i, Fixed fstN, Fixed rstN)
      | otherwise            = (i, Fixed fstN, Choices rstS)
      where
        (rstN, _)    = Set.deleteFindMin rstS
        (fstN, rstS) = Set.deleteFindMin s

solve' :: Board -> Maybe Board
solve' b
  | isDeadEnd b  = Nothing
  | isValid b    = Just b
  | otherwise    = 
    let (nextBoard, restBoard) = nextChoices b
    in solve nextBoard <|> solve restBoard

solve :: Board -> Maybe Board
solve board 
  = pruneBoard board 
  >>= solve'

solutions' :: Board -> [Board]
solutions' b
  | isDeadEnd b  = []
  | isValid b    = [b]
  | otherwise    = 
    let (b1, b2) = nextChoices b
    in solutions b1 ++ solutions b2

solutions :: Board -> [Board]
solutions board =
  case pruneBoard board of
    Just pruned -> solutions' pruned
    Nothing     -> []

hasUniqueSolution :: Board -> Bool
hasUniqueSolution = (==1) . length . take 2 . solutions

solveFromString :: String -> Maybe String
solveFromString cs 
  = parseBoard cs
  >>= solve
  >>= return . showBoard

-- Generating a Board --

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do 
  randomPosition          <- getStdRandom (randomR (0, length xs - 1))
  let (left, (a : right)) = splitAt randomPosition xs
  fmap (a :) (shuffle (left ++ right))

shuffleBoard :: Board -> IO Board
shuffleBoard (b, size@(m, n)) 
  = mapM shuffle (chunksOf n b)
  >>= mapM shuffle . chunksOf m . cols . concat
  >>= return . flip (,) size . cols . concat

getSeedRow :: Int -> IO [Int]
getSeedRow n = shuffle [1..n]

createSudokuFromFilled :: [Int] -> Board -> IO Board
createSudokuFromFilled [] b = return b
createSudokuFromFilled (n':ns) board@(board', size@(m, n)) 
  = if hasUniqueSolution board'' then
      createSudokuFromFilled ns board''
    else createSudokuFromFilled ns board
  where
    board'' = flip (,) size . chunksOf (m * n) . removeith n' . zip [1..] . concat $ board'

    removeith :: Int -> [(Int, Cell)] -> [Cell]
    removeith q ((i, c) : cs)
      | i == q    = Choices (Set.fromAscList [1..(m*n)]) : map snd cs
      | otherwise = c : removeith q cs

createFilledBoard :: Int -> Int -> [Int] -> IO Board
createFilledBoard m n seedrow = return (createMNrows seedstream, (m, n))
  where
    seedstream = seeds 1 0

    createMNrows :: [Int] -> Matrix Cell
    createMNrows 
      = map (map Fixed . take (m * n) . flip drop (cycle seedrow)) 
      . take (m * n)

    seeds :: Int -> Int -> [Int]
    seeds prev i
      | i `mod` n == 0 = prev + 1 : seeds (prev + 1) (i+1)
      | otherwise      = prev + m : seeds (prev + m) (i+1)

generateBoardAsString :: Int -> Int -> IO String
generateBoardAsString m n
  = getSeedRow (m*n) >>= \sr
  -> getSeedRow (m*m*n*n) >>= \tr
  -> createFilledBoard m n sr
  >>= shuffleBoard
  >>= createSudokuFromFilled tr
  >>= return . showBoard
  
-- Parsing a Board --

readInt :: String -> Int
readInt = read

readIntMaybe :: String -> Maybe Int
readIntMaybe = readMaybe

parseCell :: Int -> String -> Maybe Cell
parseCell mx "." = return . Choices . Set.fromAscList $ [1..mx]
parseCell mx cs  = readIntMaybe cs >>= return . Fixed

parseRow :: Int -> String -> Maybe (Row Cell)
parseRow mxn = mapM (parseCell mxn) . words

parseBoard :: String -> Maybe Board
parseBoard cs = do
  (size, sb) <- List.uncons $ lines cs
  [m, n]     <- mapM readIntMaybe . words $ size
  brd        <- mapM (parseRow (m * n)) sb
  return (brd, (m, n))

makeString3x3 :: String -> String
makeString3x3 
  = unlines 
  . ("3 3" :) 
  . map (List.intersperse ' ') 
  . chunksOf 9

-- Printing a Board --

showBoard :: Board -> String
showBoard (board, (m, n)) 
  = show m ++ " " ++ show n  ++ "\n" ++ (intersperseNL $  map row2string board)
  where
    row2string :: Show a => [a] -> String
    row2string = intersperseS m "  " . map show

    intersperseNL :: [String] -> String
    intersperseNL = intersperseS n "\n"

    intersperseS :: Int -> String -> [String] -> String
    intersperseS num str
      = concat 
      . concat
      . map (List.intersperse str) 
      . List.intersperse [str ++ str] 
      . chunksOf num

