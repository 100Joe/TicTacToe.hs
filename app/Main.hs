module Main where
  import System.Environment 
  import Data.List

  data Player = X | O
  data Cell = Occupied Player | Empty
  data CellTransform = Success [Cell] | Fail String [Cell]

  instance Show Player where
    show X = "X"
    show O = "O"

  instance Show Cell where
    show (Occupied X)     = "X"
    show (Occupied O)     = "O"
    show Empty            = " "

  instance Eq Cell where
    Occupied X == Occupied X = True
    Occupied O == Occupied O = True
    Empty == Empty           = True
    _ == _                   = False

  nextTurn :: Player -> Player
  nextTurn X = O
  nextTurn O = X

  createRow :: [Cell] -> String
  createRow row = intercalate " | " $ fmap show row

  boundryLine :: String
  boundryLine = "---------"

  createBoard :: [Cell] -> IO ()
  createBoard board = do
    putStrLn $ createRow topRow
    putStrLn boundryLine
    putStrLn $ createRow middleRow
    putStrLn boundryLine
    putStrLn $ createRow bottomRow
    where topRow  = take 3 board
          middleRow = drop 3 . take 6 $ board
          bottomRow  = drop 6 board

  getBoardIndex :: String -> Maybe Int
  getBoardIndex "T1" = Just 0
  getBoardIndex "T2" = Just 1
  getBoardIndex "T3" = Just 2
  getBoardIndex "M1" = Just 3
  getBoardIndex "M2" = Just 4
  getBoardIndex "M3" = Just 5
  getBoardIndex "B1" = Just 6
  getBoardIndex "B2" = Just 7
  getBoardIndex "B3" = Just 8
  getBoardIndex _    = Nothing

  checkIsFree ::  [Cell] -> Int -> Maybe Int
  checkIsFree board ix = if board !! ix == Empty then Just ix else Nothing

  assignCell :: String -> Player -> [Cell] -> CellTransform
  assignCell location player board =
    case getBoardIndex location >>= checkIsFree board of
      Nothing -> Fail "Invalid Player" board
      Just i -> Success (take i board ++ [Occupied player] ++ drop (i+1) board)

  isWinner :: Player -> [Cell] -> Bool
  isWinner player board =
    or [
      -- check top row
      head board == Occupied player && board !! 1 == Occupied player && board !! 2 == Occupied player,
      -- check middle row
      board !! 3 == Occupied player && board !! 4 == Occupied player && board !! 5 == Occupied player,
      -- check bottom row
      board !! 6 == Occupied player && board !! 7 == Occupied player && board !! 8 == Occupied player,
      -- check left column
      head board == Occupied player && board !! 3 == Occupied player && board !! 6 == Occupied player,
      -- check middle column
      board !! 1 == Occupied player && board !! 4 == Occupied player && board !! 7 == Occupied player,
      -- check right column
      board !! 2 == Occupied player && board !! 5 == Occupied player && board !! 8 == Occupied player,
      -- check top left -> bottom right
      head board == Occupied player && board !! 4 == Occupied player && board !! 8 == Occupied player,
      -- check bottom left -> top right
      board !! 6 == Occupied player && board !! 4 == Occupied player && board !! 2 == Occupied player
    ]

  newGame :: Player  -> [Cell] -> IO ()
  newGame player board = do
    putStrLn $ show player ++ " 's turn."
    putStrLn  "Pick a cell from T1 to B3."
    createBoard board
    putStr "\nInput: "
    cell <- getLine
    case assignCell cell player board of
      Fail err board -> do
        putStrLn err
        newGame player board
      Success newBoard -> do
        if isWinner player newBoard then do
          putStrLn  ("Winner! Player " ++ show player ++ " has won!")
          createBoard newBoard
          return ()
        else newGame (nextTurn player) newBoard

  main :: IO ()
  main = do
    putStrLn  "Let's play some TicTacToe!"
    putStrLn  "Pick Top Row(T1), Middle Row(M1), or Bottom Row(B1)!"
    let newBoard = replicate 9 Empty
    newGame X newBoard
