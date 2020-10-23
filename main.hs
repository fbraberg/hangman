import Data.List
import Data.Char

lives = 11

-- Game init
hangman :: IO ()
hangman = do
    putStrLn "Enter a word"
    word <- getLine
    let guess = ['-' | _ <- word]
    printWord word guess
    play (map toLower word) guess lives

-- Game loop
play :: String -> String -> Integer -> IO ()
play _    _     0     = lose
play word guess lives | word == guess = win
                      | otherwise     = do

        -- Player makes guess
        putStrLn "Make a guess"
        ch <- getLine

        -- New guess and lives are calculated
        let newGuess = [ if (toLower (ch!!0) == w || w == g) then w else '-' | (w, g) <- zip word guess]
        let newLives = if (toLower (ch!!0)) `elem` word then lives else lives-1

        -- Print the current word progress and lives left
        printWord word newGuess
        putStrLn $ "Lives left: " ++ show newLives ++ "\n"

        -- Recurse until end
        play word newGuess newLives

-- Player lost
lose :: IO ()
lose = do
    putStrLn "You Lost!"

-- Player won
win :: IO ()
win = do
    putStrLn "You won!"

-- Prints the current word progress
printWord :: String -> String -> IO ()
printWord word guess = do
    putStrLn $ "Word is: " ++ [if ch `elem` guess then ch else '-' | ch <- word]
