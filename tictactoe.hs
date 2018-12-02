import Data.List
import Data.Set (Set, size, fromList)

data Cell = Blank | X | O deriving (Show, Eq, Ord)
newtype Board = Board [Cell] deriving (Eq, Ord)

chunks :: Int -> [a] -> [[a]]
chunks n xs
    | length xs <= n = [xs]
    | otherwise = take n xs : chunks n (drop n xs)

instance Show Board where
    show (Board cs) =
        let rows = chunks 3 cs
            rowStrings = map (intercalate " |" . map showCell) rows
        in "\n" ++ intercalate "\n-----------\n" rowStrings ++ "\n"
        where
            showCell :: Cell -> String
            showCell Blank = "  "
            showCell X = " x"
            showCell O = " o"

fromChar :: Char -> Cell
fromChar '_' = Blank
fromChar 'x' = X
fromChar 'o' = O

fromString :: String -> Board
fromString = Board . map fromChar

fromBoard :: Board -> [Cell]
fromBoard (Board cs) = cs

allX :: [Cell] -> Bool
allX cs = fromList cs == fromList [X]

allO :: [Cell] -> Bool
allO cs = fromList cs == fromList [O]

getRows :: Board -> [[Cell]]
getRows (Board cs) = chunks 3 cs

getCols :: Board -> [[Cell]]
getCols (Board cs) = [(!!) <$> pure cs <*> [x,x+3..8] | x <- [0..2]]

getDiags :: Board -> [[Cell]]
getDiags (Board cs) = fmap ((!!) <$> pure cs <*>) [[0,4,8], [2,4,6]]

isWin :: Board -> Bool
isWin b = winRows b || winCols b || winDiags b
    where
        threeInARow :: [[Cell]] -> Bool
        threeInARow = any ((||) <$> allX <*> allO)
        winRows = threeInARow . getRows
        winCols = threeInARow . getCols
        winDiags = threeInARow . getDiags

isEnd :: Board -> Bool
isEnd b@(Board cs) = isWin b || filter (==Blank) cs == []

getAll :: Board -> Cell -> [Int]
getAll (Board cs) c = findIndices (==c) cs

getBlanks :: Board -> [Int]
getBlanks (Board cs) = findIndices (==Blank) cs

nextPlayer :: Board -> Cell
nextPlayer b
    | length (getAll b X) == length (getAll b O) = X
    | otherwise = O

move :: Board -> Int -> Board
move b@(Board cs) n = Board $ take n cs ++ [nextPlayer b] ++ drop (n + 1) cs

nextMoves :: Board -> [Board]
nextMoves b = map (move b) $ getAll b Blank

allMoves :: Board -> [Board]
allMoves b
    | isWin b = [b]
    | otherwise = b : (nextMoves b >>= allMoves)

emptyBoard :: Board
emptyBoard = Board $ replicate 9 Blank

allStates :: [Board]
allStates = allMoves emptyBoard

numStates :: Int
numStates = length allStates

allGames :: [Board]
allGames = filter isEnd allStates

numGames :: Int
numGames = length allGames

allBoards :: Set Board
allBoards = fromList allStates

numBoards :: Int
numBoards = size allBoards

main = do
    putStrLn $ "Number of states: " ++ show numStates
    putStrLn $ "Number of games: " ++ show numGames
    putStrLn $ "Number of boards: " ++ show numBoards

