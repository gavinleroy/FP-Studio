module Sudoku where
  
import Control.Applicative        ((<|>))
import Data.Function              (on)
import Data.IntSet                (IntSet)
-- import Data.Vector                (Vector)
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
  . Map.foldlWithKey' (\acc k ns -> Map.insertWith (++) ns [k] acc) Map.empty
  . List.foldl' (\acc ~(i, (Choices setns))-> 
      Set.foldl' (\acc' k ->
        Map.insertWith (++) k [i] acc') acc setns) Map.empty
  . filter (not . isFixed . snd)
  . zip [1..]

pruneCellByKnown :: IntSet -> Cell -> Maybe Cell
pruneCellByKnown ns (Choices xs) = toCell $ Set.difference xs ns
pruneCellByKnown _ x = return x

pruneRowByKnown :: Row Cell -> Maybe (Row Cell)
pruneRowByKnown cells = mapM (pruneCellByKnown knowncells) cells
  where knowncells = Set.fromList [n | Fixed n <- cells]

pruneRowByUnique :: Row Cell -> Maybe (Row Cell)
pruneRowByUnique cells = case uniqueChoices of
  [] -> return cells
  _  -> mapM pruneCellMore cells 
  where
    uniqueChoices = map Set.fromList $ uniqueCells cells
    choiceSet = Set.unions uniqueChoices

    pruneCellMore :: Cell -> Maybe Cell
    pruneCellMore cell@(Fixed _) = return cell
    pruneCellMore cell@(Choices ns)
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
      >>= return . (flip (,) size)

-- Functions for Solving the Sudoku --

nextChoices :: Board -> (Board, Board)
nextChoices (board, size@(m, n)) 
  = ( list2Board size $ replCell i fstCell cellList
    , list2Board size $ replCell i rstCell cellList)
  where
    (i, fstCell, rstCell) 
      = setCell . List.minimumBy (compare `on` (numChoices . snd))
      . filter (not . isFixed . snd) $ cellList
    cellList = zip [0..] . concat $ board

    list2Board :: Size -> [Cell] -> Board
    list2Board size'@(m,  n) l = (chunksOf (m * n) l, size')

    replCell :: Int -> Cell -> [(Int, Cell)] -> [Cell]
    replCell i newCell ((p, c):cs) 
      | i == p    = newCell : map snd cs
      | otherwise = c : replCell i newCell cs 

    setCell :: (Int, Cell) -> (Int, Cell, Cell)
    setCell (_, (Fixed _))   = error "unreachable setCell pattern"
    setCell (i, (Choices s))
      | Set.size s < 2       = error "unreachable setCell pattern"
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
    let (b1, b2) = nextChoices b
    in solve b1 <|> solve b2

solve :: Board -> Maybe Board
solve board 
  = pruneBoard board 
  >>= solve'

solveFromString :: String -> Maybe String
solveFromString cs 
  = parseBoard cs
  >>= solve
  >>= return . showBoard
  
-- Parsing a Board --

readInt :: String -> Int
readInt = read

readIntMaybe :: String -> Maybe Int
readIntMaybe = readMaybe

parseCell :: Int -> String -> Maybe Cell
parseCell mx "." = return $ Choices $ Set.fromAscList [1..mx]
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

makeString3x3 :: String -> String
makeString3x3 
  = unlines 
  . ((:) "3 3") 
  . map (List.intersperse ' ') 
  . chunksOf 9

-- Printing a Board --

showBoard :: Board -> String
showBoard (board, (m, n)) 
  = show m ++ "x" ++ show n  ++ "\n" ++ (intersperseNL $  map row2string board)
  where
    row2string = intersperseS m "  " . map show
    intersperseNL = intersperseS n "\n"
    intersperseS num str
      = concat 
      . concat
      . map (List.intersperse str) 
      . List.intersperse [str ++ str] 
      . chunksOf num

