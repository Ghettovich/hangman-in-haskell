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

numberOfWrongGuesses :: Hangman -> Int
numberOfWrongGuesses (Hangman word' guesses' guessesLeft) =
    length $ filter charNotInWord guesses'
    where charNotInWord c = c `notElem` word'

guessesInSecret :: Hangman -> Int
guessesInSecret (Hangman word' guesses' guessesLeft) =
    length $ filter charInGuesses word'
    where charInGuesses c = c `elem` guesses'

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
    let guessesLeft = maxGuesses - numberOfWrongGuesses hangman    
    let guessesCorrectLength = guessesInSecret hangman
    let secretLength = length $ secret hangman

    displayState hangman { guessesLeft = guessesLeft}
  
    if guessesCorrectLength == secretLength then do
        putStrLn $ "----------"
        putStrLn $ "Hooray, you win!"
        putStrLn $ "----------"
    else if guessesLeft == 0 then do
        putStrLn $ "----------"
        putStrLn $ "Game over"
        putStrLn $ "----------"
    else do
        line <- getLine
        let guess = line
        putStrLn $ "Guess: " ++ guess
        gameLoop $ hangman { guesses = guesses hangman ++ guess }

newGame :: IO Hangman
newGame = do
    return $ Hangman randomWord [] 6
        
main :: IO ()
main = do
    newGame >>= gameLoop