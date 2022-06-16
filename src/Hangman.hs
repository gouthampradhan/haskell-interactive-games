module Hangman
    ( startGame
    ) where

import System.IO      

startGame :: IO ()
startGame = do hSetBuffering stdout LineBuffering
               putStrLn "Think of a word:"
               word <- sgetLine
               putStrLn "Try to guess it:"
               play word

match :: String -> String -> String
match xs ys = [if x `elem` ys then x else '-' | x <- xs]

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)

play :: String -> IO ()
play word = do putStr "?"
               guess <- getLine
               if guess == word then
                  putStrLn "You got it !!"
               else
                  do putStrLn (match word guess)
                     play word