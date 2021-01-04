module Main where

import Control.Concurrent
import System.Random

data Coin = Head | Tail deriving (Show, Eq) 

-- Type defs
type Name = String
type Winner = String

coinFlip :: IO Coin
coinFlip = do
    -- use random lib to create random Bool value 
    r <- randomIO :: IO Bool
    return $ if r then Head else Tail

--process :: String -> MVar String 
process :: Name -> MVar Winner -> MVar Coin -> IO ()
process name winner box = do 
    c2 <- takeMVar box
    putStrLn $ name ++ "'s turn"
    c1 <- coinFlip 
    putStrLn $ " -- got " ++ (show c1)
    if c1 == c2 then 
        putMVar winner ("Process " ++ name ++ "wins!")
    else do 
        putStrLn $ " -- putting coin back in the box"
        putMVar box c2
        threadDelay 100
        process name winner box

main :: IO ()
main = do
    coin <- coinFlip
    putStrLn $ "Random coin is: " ++ (show coin)
    box <- newMVar coin
    winner <- newEmptyMVar
    forkIO (process "A" winner box)
    forkIO (process "B" winner box)
    forkIO (process "C" winner box)
    w <- takeMVar winner
    putStrLn $ "THE WINNER IS " ++ w
