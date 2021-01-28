module ExactCover 
  ( -- solveBoardAlgoX 
  ) where

-- import Data.Matrix       (Matrix)
-- import Sudoku            (Board, Cell)

-- data Cell = Zero | One
-- instance Show Cell where
--   show (Zero) = "0"
--   show (One) = "1"

-- type CoverMatrix = Matrix Cell
-- data CMData = { m    :: Int
--               , n    :: Int
--               , size :: Int
--               }

-- type matrTransformer = ((Int, CMData, CoverMatrix) -> (Int, CMData, CoverMatrix))


-- indexInCM :: CMData   -- Matrix Data
--           -> Int      -- Row
--           -> Int      -- Col
--           -> Int      -- Num
--           -> Int      -- Return Index
-- indexInCM CMData{size = s} row col n
--   = (row - 1) * s * s + (col - 1) * s + (n - 1)

-- rowCons :: matrTransformer
-- rowCons (n, (cmdat@CMData{ size = s }), cmat) 
--   = undefined

-- blockCons :: matrTransformer
-- blockCons (n, (cmdat@CMData{ size = s }), cmat) 
--   = undefined

-- colCons :: matrTransformer
-- colCons (n, (cmdat@CMData{ size = s }), cmat) 
--   = undefined

-- cellCons :: matrTransformer
-- cellCons (h, (cmdat@CMData{ size = s }), cmat) 
--   -- TODO sequence over these functions to produce the final output
--   = [(setElem One (i, hd)) 
--     | row <- [1..s]
--     , col <- [1..s]
--     , n   <- [1..s]
--     , hd  <- [h..]
--     , let i = indexInCM cmdat row col n
--     ]

-- createCoverMatrix :: Board -> CoverMatrix
-- createCoverMatrix (b, (mm, nn)) = cmatrix
--   where 
--     (_, _, cmatrix)     = blockCons $ colCons $ rowCons $ cellCons 0 cmdata emptyMatrix
--     cmdata              = CMData { m = mm, n = nn, size = msize }
--     emptyMatrix         = matrix (msize * msize * msize) (msize * msize * 4) (\_ -> Zero)
--     msize               = n * m

-- {- Knuth's Algorithm X for Exact Cover
--  - 1. Convert the Sudoku Board to a Cover Matrix
--  -  1.a Create Blank Cover Matrix
--  - 2. Create the Quadruple Chained Linked List
--  - 3. Solve
--  - 4. Convert back to a Sudoku Board
--  - -}
-- solveBoardAlgoX :: Board -> Board
-- solveBoardAlgoX = undefined



