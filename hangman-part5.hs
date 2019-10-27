data Hangman = Hangman { secret :: String, guesses:: String, guessesLeft :: Int}

randomWord :: String
randomWord = "doktor"

maxGuesses :: Int
maxGuesses = 6

drawStickFigure :: Int -> String
drawStickFigure x = unlines $ case x of
    6 -> ["=========", "|    |", "|   ", "|   ", "|   "]
    5 -> ["=========",
          "|    |",
          "|   " ++ " O ",
          "|   ",
          "|   "]
    4 -> ["=========",
          "|    |",
          "|   " ++ " O ",
          "|   " ++ " |  "]
    3 -> ["=========",
          "|    |",
          "|   " ++ " O ",
          "|   " ++ " | ",
          "|   " ++ "/  "]
    2 -> ["=========",
          "|    |",
          "|   " ++ " O ",
          "|   " ++ " | ",
          "|   " ++ "/ \\"]
    1 -> ["=========",
          "|    |",
          "|   " ++ "_O ",
          "|   " ++ " | ",
          "|   " ++ "/ \\"]
    0 -> ["=========",
          "|    |",
          "|   " ++ "_O_",
          "|   " ++ " | ",
          "|   " ++ "/ \\"]


displayState :: Hangman -> IO ()
displayState (Hangman word' guesses' guessesLeft) = do

    let stickFig = drawStickFigure guessesLeft
    let wordWithGuesses = map(\c -> if c `elem` guesses' then c else '_')

    putStrLn $ stickFig
    putStrLn $ "Word to guess: " ++ wordWithGuesses word'
    putStrLn $ "Guesses: " ++ guesses'
    putStrLn $ "Guesses left: " ++ show guessesLeft

gameLoop :: Hangman -> IO ()
gameLoop hangman = do
      displayState hangman
      if maxGuesses == 0 then do
        putStrLn $ "Game over"
      else do
        line <- getLine
        let guess = line
        putStrLn $ "Guess: " ++ guess
        gameLoop $ hangman { guesses = guesses hangman ++ guess }
            

newGame :: IO Hangman
newGame = do
    return $ Hangman randomWord [] maxGuesses
        
main :: IO ()
main = do
    newGame >>= gameLoop