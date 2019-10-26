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

displayState word' guesses' guessesLeft = do
    let stickFig = drawStickFigure guessesLeft
    let wordWithGuesses = map(\c -> if c `elem` guesses' then c else '_')

    putStrLn $ stickFig
    putStrLn $ "Word to guess: " ++ wordWithGuesses word'
    putStrLn $ "Guesses: " ++ guesses'
    putStrLn $ "Guesses left: " ++ show guessesLeft

main = putStr $ drawStickFigure 6