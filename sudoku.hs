
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Char (ord, chr)
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
    Maybe.maybe '.' intToChar $ Map.lookup (linenum, colnum) sdk

sudokuToString :: Sudoku -> String
sudokuToString sdk =
    unlines [[getSudokuChar linenum colnum sdk | colnum <- [1..9]] | linenum <- [1..9]]

debugSudoku :: Sudoku -> IO ()
debugSudoku sdk =
    mapM_ putStrLn [show $ (i,j,IntSet.toList $ getPossibilitiesForEmptySquare sdk (i,j)) | i <- [1..9], j <- [1..9]]

printSudoku :: Sudoku -> IO ()
printSudoku = putStrLn . sudokuToString

printSudokuAll :: [Sudoku] -> IO ()
printSudokuAll = mapM_ printSudoku

-- REAL STUFF

getValuesOnRow :: Sudoku -> Int -> Int -> IntSet.IntSet
getValuesOnRow sdk l c =
    IntSet.fromList $ Maybe.catMaybes [Map.lookup (l, c) sdk | c <- [1..9]]

getValuesOnCol :: Sudoku -> Int -> Int -> IntSet.IntSet
getValuesOnCol sdk l c =
    IntSet.fromList $ Maybe.catMaybes [Map.lookup (l, c) sdk | l <- [1..9]]

roundToCenter :: Int -> Int
roundToCenter n =
    2 + 3 * ((n-1) `div` 3)

getValuesIn3x3 :: Sudoku -> Int -> Int -> IntSet.IntSet
getValuesIn3x3 sdk l c =
    let l_center = roundToCenter l
        c_center = roundToCenter c in
        IntSet.fromList $ Maybe.catMaybes [Map.lookup (l_center + i, c_center + j) sdk | i <- [-1..1], j <- [-1..1]]

getPossibilitiesForEmptySquare :: Sudoku -> (Int, Int) -> IntSet.IntSet
getPossibilitiesForEmptySquare sdk (l,c) =
    IntSet.difference (IntSet.fromList [1..9]) (IntSet.unions [(getValuesOnRow sdk l c), (getValuesOnCol sdk l c), (getValuesIn3x3 sdk l c)])

emptySquares :: Sudoku -> [(Int, Int)]
emptySquares sdk =
    Prelude.filter ((flip notMember) sdk) [(i,j) | i <- [1..9],j <- [1..9]]

isComplete :: Sudoku -> Bool
isComplete sdk =
    Map.size sdk == 81

insertIntoSudoku :: Sudoku -> (Int, Int) -> Int -> Sudoku
insertIntoSudoku sdk (i,j) n =
    Map.insert (i,j) n sdk

solveSudokuAll :: Sudoku -> [Sudoku]
solveSudokuAll sdk
    | isComplete sdk = [sdk]
    | otherwise = let emptySq = head $ emptySquares sdk in
            Prelude.concatMap solveSudokuAll $ Prelude.map (insertIntoSudoku sdk emptySq) $ IntSet.toList $ getPossibilitiesForEmptySquare sdk emptySq

solveSudoku :: Sudoku -> Sudoku
solveSudoku = head . solveSudokuAll

-- MAIN

main = do
    readFile "sudoku2.txt" >>= (printSudokuAll . solveSudokuAll . loadSudoku)