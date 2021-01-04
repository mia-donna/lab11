module Main where

import Control.Concurrent
import System.Random

data Coin = Head | Tail deriving (Show, Eq) 

coinFlip :: IO Coin
coinFlip = do
    r <- randomIO :: IO Bool
    return $ if r then Head else Tail

main :: IO ()
main = do
    c <- coinFlip
    print c
