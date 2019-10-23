data Hangman = Hangman { secret :: String, guesses:: String, guessesLeft :: Int}

randomWord :: String
randomWord = "schildpad"

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
displayState (Hangman word' guesses' guessesLeft)  =
    putStrLn $ (drawStickFigure guessesLeft) ++ unlines 
            [ ""
            ,"Word to guess: " ++ wordWithGuesses
            , ""
            , "Guesses: " ++ guesses'
            , "Guesses left: " ++ show guessesLeft
            ]
    where
        gameState = Hangman word' guesses' guessesLeft
        wordWithGuesses = blankOrChar <$> word'        
        blankOrChar c
            | c `elem` guesses' = c
            | otherwise = '_'

newGame :: IO Hangman
newGame = do
    return $ Hangman randomWord [] maxGuesses
        
main :: IO ()
main = do
    newGame >>= displayState