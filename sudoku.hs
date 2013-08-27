
import Data.Map
import Data.Maybe
import Data.Char
import qualified Data.IntSet as IntSet

type Sudoku = Map (Int,Int) Int

charToInt :: Char -> Int
charToInt c = ord c - ord '0'

intToChar :: Int -> Char
intToChar i = chr $ i + 48

-- INPUT

loadSudokuSquare :: Int -> Int -> Char -> Sudoku
loadSudokuSquare linenum colnum c
    | c == '.' = empty
    | ('1' <= c) && (c <= '9') = singleton (linenum, colnum) $ charToInt c
    
loadSudokuLine :: Int -> String -> Sudoku
loadSudokuLine linenum s = 
    unions $ [loadSudokuSquare linenum colnum c | (colnum,c) <- zip [1..9] s]

loadSudoku :: String -> Sudoku
loadSudoku s =
    unions $ [loadSudokuLine linenum s | (linenum, s) <- zip [1..9] (lines s)]

-- OUTPUT

getSudokuChar :: Int -> Int -> Sudoku -> Char
getSudokuChar linenum colnum sdk =
    Data.Maybe.maybe '.' intToChar $ Data.Map.lookup (linenum, colnum) sdk

sudokuToString :: Sudoku -> String
sudokuToString sdk =
    unlines [[getSudokuChar linenum colnum sdk | colnum <- [1..9]] | linenum <- [1..9]]

printSudoku :: Sudoku -> IO ()

-- REAL STUFF

getRow sdk l c =
    IntSet.fromList $ Data.Maybe.catMaybes [Data.Map.lookup (l, colnum) sdk | colnum <- [1..9]]

getCol sdk l c =
    IntSet.fromList $ Data.Maybe.catMaybes [Data.Map.lookup (l, colnum) sdk | colnum <- [1..9]]

getAround sdk l c =

getPossibilitiesForEmptySquare sdk l c =

solveSudokuAll :: Sudoku -> [Sudoku]
solveSudokuAll sdk =
    [sdk]

solveSudoku :: Sudoku -> Sudoku
solveSudoku = head . solveSudokuAll

printSudoku = putStrLn . sudokuToString

main = do
    readFile "sudoku.txt" >>= (printSudoku . loadSudoku)