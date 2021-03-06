{-# LANGUAGE BlockArguments #-} 

module Main where

import Control.Concurrent  ( threadDelay, forkIO , takeMVar , putMVar , newEmptyMVar , MVar , newMVar , readMVar )
import Text.Printf ( printf )
import Control.Monad ( forever )
import System.IO
import Prelude hiding (lookup)
import Data.Map (Map, (!) )
import Data.Set as Set
import qualified Data.Map as Map
import Control.Concurrent.STM
import Data.Maybe
import System.Random

{- chan.hs
-- <<Stream
type Stream a = MVar (Item a)
data Item a   = Item a (Stream a)
-- >>

-- <<Chan
data Chan a
 = Chan (MVar (Stream a))
        (MVar (Stream a))
-- >>

-- <<newChan
newChan :: IO (Chan a)
newChan = do
  hole  <- newEmptyMVar
  readVar  <- newMVar hole
  writeVar <- newMVar hole
  return (Chan readVar writeVar)
-- >>

-- <<writeChan
writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ writeVar) val = do
  newHole <- newEmptyMVar
  oldHole <- takeMVar writeVar
  putMVar oldHole (Item val newHole)
  putMVar writeVar newHole
-- >>

-- <<readChan
readChan :: Chan a -> IO a
readChan (Chan readVar _) = do
  stream <- takeMVar readVar            -- <1>
  Item val tail <- takeMVar stream      -- <2>
  putMVar readVar tail                  -- <3>
  return val
-- >>

-- <<dupChan
dupChan :: Chan a -> IO (Chan a)
dupChan (Chan _ writeVar) = do
  hole <- takeMVar writeVar
  putMVar writeVar hole
  newReadVar <- newMVar hole
  return (Chan newReadVar writeVar)
-- >>
-- <<unGetChan
unGetChan :: Chan a -> a -> IO ()
unGetChan (Chan readVar _) val = do
  newReadEnd <- newEmptyMVar             -- <1>
  readEnd <- takeMVar readVar            -- <2>
  putMVar newReadEnd (Item val readEnd)  -- <3>
  putMVar readVar newReadEnd             -- <4>
-- >>

main = do
  c <- newChan
  writeChan c 'a'
  readChan c >>= print
  c2 <- dupChan c
  writeChan c 'b'
  readChan c >>= print
  readChan c2 >>= print
-}






{-
-- phonebook.hs 
-- <<types
type Name        = String
type PhoneNumber = String
type PhoneBook   = Map Name PhoneNumber

newtype PhoneBookState = PhoneBookState (MVar PhoneBook)
-- >>

-- <<new
new :: IO PhoneBookState
new = do
  m <- newMVar Map.empty
  return (PhoneBookState m)
-- >>

-- <<insert
insert :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert (PhoneBookState m) name number = do
  book <- takeMVar m
  putMVar m (Map.insert name number book)
-- >>

-- <<lookup
lookup :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookup (PhoneBookState m) name = do
  book <- takeMVar m
  putMVar m book
  return (Map.lookup name book)
-- >>

-- <<main
main = do
  s <- new
  sequence_ [ insert s ("name" ++ show n) (show n) | n <- [1..10000] ]
  lookup s "name8" >>= print
  lookup s "unknown" >>= print
-- >>
-}


{- logger.hs
-- <<Logger - MVar that we use for communication with the logging thread that takes a logcommand 
-- requests are made by placing a logcommand in the MVar
-- and the logging thread will process them one at a time by taking them from the MVar
data Logger = Logger (MVar LogCommand)

-- Two commands that we can make, so String takes the message and Stop represents the message requesting the logging thread to terminate
data LogCommand = Message String | Stop (MVar ())
-- >>

-- <<initLogger creates a new logging service
initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  forkIO (logger l)
  return l
-- >>

-- <<logger once we've created an empty MVar and forked a thread to perform the service -
-- the thread will run the function logger:
logger :: Logger -> IO ()
logger (Logger m) = loop
 where
  loop = do
    cmd <- takeMVar m
    case cmd of
      Message msg -> do
        putStrLn msg
        loop
      Stop s -> do
        putStrLn "logger: stop"
        putMVar s ()
-- >>

-- <<logMessage is a function that the client uses to log a message
logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)
-- >>

-- <<logStop
logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s
-- >>

-- <<main
main :: IO ()
main = do
  l <- initLogger
  logMessage l "hello"
  logMessage l "bye"
  logStop l
                          
-- >>

-}

{-
--mvar 3
main :: IO ()
main = do
  m <- newEmptyMVar
  takeMVar m
-}

{-
--mvar2.hs
main :: IO ()
main = do
  m <- newEmptyMVar
  forkIO $ do putMVar m 'x'; putMVar m 'y'
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r
-}

{- mvar1.hs
-- <<main
main :: IO ()
main = do
  m <- newEmptyMVar
  forkIO $ putMVar m 'x'
  r <- takeMVar m
  print r
-- >>
-}


{--- reminder.hs (with 2 updates - recursive loop on main -- we can terminate the program even if there are outstanding reminders)
main :: IO ()
main = loop
  where 
    loop = do
     s <- getLine           -- <1>  Waits for input from the user
     if s == "exit" 
        then return ()
        else do forkIO $ setReminder s -- <2>  Creates a new thread to handle this reminder
                loop

setReminder :: String -> IO ()
setReminder s  = do
  let t = read s :: Int
  printf "Ok, I'll remind you in %d seconds\n" t
  threadDelay (10^6 * t)                   -- <3> The new thread, after printing a confirmation message, waits for the spec seconds in threadDelay
  printf "%d seconds is up! BING!\BEL\n" t -- <4> Finally, when threadDelay returns, the reminder message is printed
-- >>
-}


{-
-- fork.hs 
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering            -- <1> This puts the output HANDLE into nonbuffered mode, so that we can see the interleavign more clearly
  forkIO (replicateM_ 100000 (putChar 'A'))   -- <2> Creates a thread to print the character A x 100000
  replicateM_ 100000 (putChar 'B')            -- <3> In the main thread, print the character B x 100000
-- >>
-}

{-mapIntToDice :: Int -> Dice
mapIntToDice n = case n of
    0 -> One
    1 -> Two
    2 -> Three
    3 -> Four
    4 -> Five
    5 -> Six
    where r = mod n 6-}

{-
process :: String -> MVar String -> MVar Coin -> IO () 
process name winner box = do 
    c1 <- takeMVar box
    putStrLn $ name ++ "'s turn"
    c2 <- coinFlip 
    putStrLn $ " -- got " ++ (show c2)
    if c1 == c2 then 
        putMVar winner $ "Process " ++ name ++ "wins!"
    else do 
        putStrLn $ " -- putting coin back in the box"
        putMVar box c1
        threadDelay 100
        process name winner box
-}

{-
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
    putStrLn $ w
-}

{-main :: IO ()
main = do
    putStrLn $ "hi"
-}





{-
-- FIRST COIN FLIP GAME WITH NOTES ******************************************************************************
-- datatype
data Coin = Head | Tail  deriving (Show, Eq)
type Name = String
type Winner = String

-- function - gets one of the two values at random
coinFlip :: IO Coin
coinFlip = do 
    r <- randomIO :: IO Bool
    return $ if r then Head else Tail
    -- if true head, if false tail

-- define the functions that are going to be the implementations of our threads 
-- three different threads and they're all playing the game 
-- define the thread -> thread is a process/process is a thread
-- give each thread a name of the player, then two boxes that each thread will have access to
-- MVar String (name of winner) -- MVar Coin (coin that was initially flipped)
-- 1) we will flip a coin, 2) then the players will take turns to try to get the same value as the initial coin
-- needs to be IO() to spawn a thread
-- 

process :: Name-> MVar Winner -> MVar Coin -> IO ()
process name winner box = do 
    -- we will take a coin value from the box so that other process is blocked
    -- if there is someone already took the coin, this thread will hang + waiting until something in the box
    c2 <- takeMVar box 
    -- then the thread flips its own coin 
    putStrLn $ name ++ "'s turn"
    c1 <- coinFlip
    -- then we compare the two coins
    -- each process needs to 1. TAKE A COIN 2. CHECK if its the same in the BOX 3. IF YES == WINNER and stop threads 4. IF NO the put COIN INTO BOX 
    putStrLn $ name ++ "--- got " ++ (show c1)  
    if c1 == c2 then
        -- we can declare this thread the winner -- puts in the winner box the message that says "i've won"
        putMVar winner ("Process " ++ name ++ " wins!")
        -- if not winner then need to do a few things
    else do 
        -- put it back in the box so the next thread can have a go (this is where we checked to see if it was the same as our flip)
        putStrLn $ "putting coin back in the box"
        putMVar box c2
        threadDelay 100 
        -- wait a bit until we can repeat the process
        process name winner box

main :: IO ()
main = do
    -- Flip the original coin
    coin <- coinFlip
    putStrLn $ "Random coin is: " ++ (show coin)
    -- put coin in the main box (main box is box) and starts with a value (newMVar)
    box <- newMVar coin 
    -- create an empty box for the winner (newEmptyMVar)
    winner <- newEmptyMVar 
    -- fork the three processes, with the winner box and the coin box
    forkIO (process "A" winner box)
    forkIO (process "B" winner box)
    forkIO (process "C" winner box)
    -- so that these processes can interact, before the main thread ends, block the main thread
    -- we need a way to ensure the main thread waits so the 3 players can play
    -- because this box is initially empty, this will block the main thread
    -- main thread is waiting until 1 player declares themselves the winner
    -- then the winner will be filled and the main thread can take the winner from the box
    w <- takeMVar winner
    putStrLn $ "The winner is: " ++ w
-- END OF FIRST COIN FLIP GAME WITH NOTES ******************************************************************************

-}




{-
-- SECOND COIN FLIP GAME WITH NOTES ******************************************************************************

-- here i've updated the create thread function "process" which includes an original data type Account and the Account Number printed


-- datatype
data Coin = Head | Tail  deriving (Show, Eq)
type Name = String
type Winner = String

-- should I be creating a fucntion to change customer names to string? so we can print hem out
--customerNames :: Customer -> String
--customerNames customer = name customer : Map.map name (account customer)
     
-- function - gets one of the two values at random
coinFlip :: IO Coin
coinFlip = do 
    r <- randomIO :: IO Bool
    return $ if r then Head else Tail
    -- if true head, if false tail

-- define the functions that are going to be the implementations of our threads 
-- three different threads and they're all playing the game 
-- define the thread -> thread is a process/process is a thread
-- give each thread a name of the player, then two boxes that each thread will have access to
-- MVar String (name of winner) -- MVar Coin (coin that was initially flipped)
-- 1) we will flip a coin, 2) then the players will take turns to try to get the same value as the initial coin
-- needs to be IO() to spawn a thread

data Account = One | Two | Three deriving (Show, Eq)

data Customer = Customer {
  name :: Name,
  --accountBalance :: AccountBalance,
  account :: Account
} deriving (Eq, Show)


process :: Name -> Account -> MVar Winner -> MVar Coin -> IO ()
process name account winner box = do 
    -- we will take a coin value from the box so that other process is blocked
    -- if there is someone already took the coin, this thread will hang + waiting until something in the box
    c2 <- takeMVar box 
    -- then the thread flips its own coin 
    putStrLn $ name ++ "'s turn"
    c1 <- coinFlip
    -- then we compare the two coins
    -- each process needs to 1. TAKE A COIN 2. CHECK if its the same in the BOX 3. IF YES == WINNER and stop threads 4. IF NO the put COIN INTO BOX 
    putStrLn $ name ++ "--- got " ++ (show c1)  
    if c1 == c2 then
        -- we can declare this thread the winner -- puts in the winner box the message that says "i've won"
        putMVar winner ("Process " ++ name ++ " wins!" ++ " Their account number is: " ++ (show account) )
        -- if not winner then need to do a few things
    else do 
        -- put it back in the box so the next thread can have a go (this is where we checked to see if it was the same as our flip)
        putStrLn $ "putting coin back in the box"
        putMVar box c2
        threadDelay 100 
        -- wait a bit until we can repeat the process
        process name account winner box

main :: IO ()
main = do
    -- Flip the original coin
    coin <- coinFlip
    putStrLn $ "Random coin is: " ++ (show coin) 
    -- put coin in the main box (main box is box) and starts with a value (newMVar)
    box <- newMVar coin 
    -- create an empty box for the winner (newEmptyMVar)
    winner <- newEmptyMVar 
    -- fork the three processes, with the winner box and the coin box
    forkIO (process "A" One winner box)
    forkIO (process "B" Two winner box)
    forkIO (process "C" Three winner box)
    -- so that these processes can interact, before the main thread ends, block the main thread
    -- we need a way to ensure the main thread waits so the 3 players can play
    -- because this box is initially empty, this will block the main thread
    -- main thread is waiting until 1 player declares themselves the winner
    -- then the winner will be filled and the main thread can take the winner from the box
    w <- takeMVar winner
    putStrLn $ "The winner is: " ++ w
-- END OF SECOND COIN FLIP GAME WITH NOTES ******************************************************************************

-}




-- CUSTOMER PROCESS NEEDS TO INCLUDE: remember this is a customer
--1. take something from the box to clear it?
   -- x <- takeMVar box
--2. declare WHICH customer it is that's running the customer thread process
  -- putStrLn $ Customer name "'s turn"   (we do need a name here in the process)
-- select one of the other customers at random (1 random customer selection func)
   -- putStrLn $ "Selecting a random customer to transfer funds to..."
   -- y <- selectRandomCustomer
   -- putStrLn $ name ++ "--- got " ++ (show y)
-- transfer a random amount of money (between £10 and £50) (random number 10:50 func )
   -- amount <- randomNumber10:50
   -- transfer(function) amount to y


   --TO TRY !!!!!!!!!!!!!!
   -- Create a "random selector" that selects customer threads..?

-- you have to give, when you run the process in main and create the fork, the customer details (or the account details)
-- so maybe we create a customer (eg let c1 = c1 data ) then we forkIO process "A"--ID One--Account c1--Customer info box-- box
-- basically we want to try to show customer data in a thread (we can transfer and we can select account numbers) -- but we want to select a customer datapoint from a thread process
-- we can do it with a thread, but not a process yet


 {- -- THIRD COIN FLIP GAME WITH NOTES ******************************************************************************

-- here i've updated the create thread function "process" which includes CUSTOMER and the ability to print CUSTOMER!!!
-- next I need to try to make two threads inetract, and try transfering funds? so i've added accountBalance, next to try to add a transfer
-- stopping third game progress to restruture it a bit and creating a fourth

-- datatype
data Coin = Head | Tail  deriving (Show, Eq)
type Name = String
type FirstWinner = String
type SecondWinner = String
type AccountBalance = Int
     
-- function - gets one of the two values at random
coinFlip :: IO Coin
coinFlip = do 
    r <- randomIO :: IO Bool
    return $ if r then Head else Tail
    -- if true head, if false tail

-- define the functions that are going to be the implementations of our threads 
-- three different threads and they're all playing the game 
-- define the thread -> thread is a process/process is a thread
-- give each thread a name of the player, then two boxes that each thread will have access to
-- MVar String (name of winner) -- MVar Coin (coin that was initially flipped)
-- 1) we will flip a coin, 2) then the players will take turns to try to get the same value as the initial coin
-- needs to be IO() to spawn a thread

data Account = One | Two | Three deriving (Show, Eq)

data Customer = Customer {
  name :: Name,
  accountBalance :: AccountBalance,
  account :: Account
} deriving (Eq, Show)

process :: Name -> Account -> Customer -> MVar FirstWinner -> MVar SecondWinner -> MVar Coin -> IO ()
process name account cust firstwinner secondwinner box = do 
    -- we will take a coin value from the box so that other process is blocked
    -- if there is someone already took the coin, this thread will hang + waiting until something in the box
    c2 <- takeMVar box 
    -- then the thread flips its own coin 
    putStrLn $ "Step 2. A random thread I created in main will go first (according to the machines OS) - this thread will now flip a coin. Let's see which thread was randomly chosen.. "
    putStrLn $ "Customer " ++ name ++ " is chosen to play first"
    c1 <- coinFlip
    -- then we compare the two coins
    -- each process needs to 1. TAKE A COIN 2. CHECK if its the same in the BOX 3. IF YES == WINNER and stop threads 4. IF NO the put COIN INTO BOX 
    putStrLn $ name ++ "--- got " ++ (show c1)
    putStrLn $ "Step 3. If the coin's match - then this thread becomes the payee. If not, then we will flip the original coin again."  
    if c1 == c2 then do
        putStrLn $ "Woop! There was a match between the coin in the box and the coin the first process got - so they will get the transfer"
        -- we can declare this thread the winner -- puts in the winner box the message that says "i've won"
        putMVar firstwinner ("Customer Thread Process " ++ name ++ " matches! So they get the transfer..." ++ " Their customer account details are: " ++ (show cust) )
        --i <- takeMVar firstwinner
        -- ****NEW**** ADD A TRANSFER FOR THE WINNER
        --amount <- randomN
        -- choose a random account to transfer into
        -- we can declare this thread the second winner -- puts in the winner box the message that says "i've won"
        putStrLn $ "putting coin back in the box to find the payee - which is the next customer to flip the same as the original coin"
        putMVar box c2
        threadDelay 100
        else if c1 /= c2 then do
            putMVar secondwinner ("Customer Thread Process " ++ name ++ " is our second winner! So they complete the transfer..." ++ " Their customer account details are: " ++ (show cust) )
        -- if not winner then need to do a few things
        else do
        -- put it back in the box so the next thread can have a go (this is where we checked to see if it was the same as our flip)
            putStrLn $ "putting coin back in the box"
            putMVar box c2
            threadDelay 100 
        -- wait a bit until we can repeat the process
            process name account cust firstwinner secondwinner box

main :: IO ()
main = do
    -- Flip the original coin
    coin <- coinFlip
    putStrLn $ "Step 1: Flip a random coin - let's call this first original random coin "
    putStrLn $ "First original random coin is: " ++ (show coin) 
    -- put coin in the main box (main box is box) and starts with a value (newMVar)
    box <- newMVar coin 
    -- create an empty box for the winner (newEmptyMVar)
    firstwinner <- newEmptyMVar
    secondwinner <- newEmptyMVar 
    -- HAVE TO create 3 customers first THEN add them to the THREAD function THEN fork them!!!

    let c1 = Customer {name = "C1", accountBalance = 100, account = One}
    let c2 = Customer {name = "C2", accountBalance = 100, account = Two} 
    let c3 = Customer {name = "C3", accountBalance = 100, account = Three}

    -- fork the three processes, with the winner box and the coin box
    forkIO (process "A" One c1 firstwinner secondwinner box)
    forkIO (process "B" Two c2 firstwinner secondwinner box)
    forkIO (process "C" Three c3 firstwinner secondwinner box)

    -- so that these processes can interact, before the main thread ends, block the main thread
    -- we need a way to ensure the main thread waits so the 3 players can play
    -- because this box is initially empty, this will block the main thread
    -- main thread is waiting until 1 player declares themselves the winner
    -- then the winner will be filled and the main thread can take the winner from the box
    w <- takeMVar firstwinner
    z <- takeMVar secondwinner
    putStrLn $ "The winner is: " ++ w
    -- ****NEW**** ADD A TRANSFER FOR THE FIRST WINNER
    amount <- randomN
    -- choose a random account to transfer into
    putStrLn $ "The winning customer gets: £" ++ (show amount)
    
    putStrLn $ "Now we will find the payee.. "

-- get any random number between 10 : 50
randomN :: IO Int 
randomN = do
    r <- randomRIO (10, 50)
    return r

transfer :: Customer -> Customer -> Int -> IO (Customer, Customer)
-- transfer from an acount to another account a desired amount
transfer from to amount
  | amount <= 0 = return (from, to)
  | accountBalance from < amount = return (from, to)
  | otherwise = return ((from { accountBalance =  ((accountBalance from) - amount)}),(to { accountBalance =  ((accountBalance to) + amount)}))

-- im adding a first and second winner, to try to run the game again to get a second process to transfer to



-- END OF THIRD COIN FLIP GAME WITH NOTES ******************************************************************************   -}



{-
-- START OF FOURTH COIN FLIP GAME WITH NOTES ******************************************************************************   
-- Sunday 10.01.21
-- Aim: to restructure the process so the interaction works propery

data Coin = Head | Tail  deriving (Show, Eq)
type Name = String
type FirstWinner = String
type SecondWinner = String
type AccountBalance = Int
     
coinFlip :: IO Coin
coinFlip = do 
    r <- randomIO :: IO Bool
    return $ if r then Head else Tail


data Account = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten deriving (Show, Eq)

data Customer = Customer {
  name :: Name,
  accountBalance :: AccountBalance,
  account :: Account
} deriving (Eq, Show)

customerthread :: Customer -> MVar (Customer, FirstWinner) -> MVar SecondWinner -> MVar Coin -> IO ()
customerthread cust firstwinner secondwinner box = do 
    coin <- takeMVar box 
 -- Step 2. A random thread will play - this thread will now flip a coin. 
    putStrLn $ "Customer: " ++ (show cust) ++ " is chosen to play. "
    coin1 <- coinFlip
    putStrLn $ "They got --- " ++ (show coin1)
-- Step 3. If the coin's match - then this thread becomes the payee. If not, then we will flip the original coin again. If we are on step 3 for the second time - then we're looking for the depositer!    
    -- AIM: to find two accounts
    if coin1 == coin then do
        putStrLn $ "Woop! There was a match between the coin in the box and the coin the customer got - so Customer: " ++ (show cust) ++  " will get the transfer"
       
        --putMVar firstwinner ("Customer: " ++ (show cust) ++ " matches! So they get the transfer..." ++ " Their customer account details are: " ++ (show cust) )
        -- commented out the above and trying to put the customer in the box with the first winner
        
        putMVar firstwinner (cust, ("Customer: " ++ (show cust) ++ " matches! So they get the transfer..." ++ " Their customer account details are: " ++ (show cust) ))
        putStrLn $ "Now putting coin back in the box to find the payee - which is the next random Customer Thread to flip the same as the original coin"
        putMVar box coin1
        threadDelay 100
        -- need to find second winner 
        putStrLn $ "Customer " ++ (show cust) ++ " is chosen to play next"
        {-coin1 <- coinFlip
        putStrLn $ name ++ "--- gottt " ++ (show coin1)
        else if coin1 == coin then do
           putStrLn $ "another match!"
           putMVar secondwinner ("Customer Thread Process " ++ name ++ " is our second winner! So they complete the transfer..." ++ " Their customer account details are: " ++ (show cust) )
        -}
       -- else if c1 == c2 then do
          --  putStrLn $ "YEAHH we've got two winners - now we can do a transfer"
        --else if c1 /= c2 then do
          --  c2 <- takeMVar box
           -- putStrLn $ "terminating"
       {-else if c1 /= c2 then do
            putStrLn $ "Ah. There was no match. Putting coin back in the box to try again"
            putMVar box c2
            threadDelay 100
            process name account cust firstwinner secondwinner box 
            putMVar secondwinner ("Customer Thread Process " ++ name ++ " is our second winner! So they complete the transfer..." ++ " Their customer account details are: " ++ (show cust) )
        -}
        else do
        -- put it back in the box so the next thread can have a go (this is where we checked to see if it was the same as our flip)
            putStrLn $ "no match this time - putting coin back in the box to try again"
            putMVar box coin
            threadDelay 100 
        -- wait a bit until we can repeat the process
            customerthread cust firstwinner secondwinner box

main :: IO ()
main = do
    coin <- coinFlip
    --Step 1: Flip a random coin - let's call this first original random coin 
    putStrLn $ "ORIGINAL COIN IS: " ++ (show coin)

    -- put coin in the main box (main box is box) and starts with a value (newMVar)
    box <- newMVar coin 

    -- create an empty box for the  winners (newEmptyMVar)
    firstwinner <- newEmptyMVar
    secondwinner <- newEmptyMVar

    -- HAVE TO create 3 customers first THEN add them to the THREAD function THEN fork them!!!

    let c1 = Customer {name = "C1", accountBalance = 100, account = One}
    let c2 = Customer {name = "C2", accountBalance = 100, account = Two} 
    let c3 = Customer {name = "C3", accountBalance = 100, account = Three}
    let c4 = Customer {name = "C4", accountBalance = 20, account = Four}
    let c5 = Customer {name = "C5", accountBalance = 20, account = Five}
    let c6 = Customer {name = "C6", accountBalance = 20, account = Six}
    let c7 = Customer {name = "C3", accountBalance = 20, account = Seven}
    let c8 = Customer {name = "C8", accountBalance = 20, account= Eight}
    let c9 = Customer {name = "C9", accountBalance = 20, account = Nine}
    let c10 = Customer {name = "C10", accountBalance = 20, account = Ten}

    -- fork the customer processes, with the winner box and the coin box
    forkIO (customerthread c1 firstwinner secondwinner box)
    forkIO (customerthread c2 firstwinner secondwinner box)
    forkIO (customerthread c3 firstwinner secondwinner box)
    forkIO (customerthread c4 firstwinner secondwinner box)
    forkIO (customerthread c5 firstwinner secondwinner box)
    forkIO (customerthread c6 firstwinner secondwinner box)
    forkIO (customerthread c7 firstwinner secondwinner box)
    forkIO (customerthread c8 firstwinner secondwinner box)
    forkIO (customerthread c9 firstwinner secondwinner box)
    forkIO (customerthread c10 firstwinner secondwinner box)
     

    w <- takeMVar firstwinner
    z <- takeMVar secondwinner
    --putStrLn $ "The winner is: " ++ w
    -- ****NEW**** ADD A TRANSFER FOR THE FIRST WINNER
    amount <- randomN
    -- choose a random account to transfer into
    putStrLn $ "The winning customer gets: £" ++ (show amount)
    putStrLn $ "The first winner is: " ++ (show w)
    putStrLn $ "exit "

-- get any random number between 10 : 50
randomN :: IO Int 
randomN = do
    r <- randomRIO (10, 50)
    return r

transfer :: Customer -> Customer -> Int -> IO (Customer, Customer)
-- transfer from an acount to another account a desired amount
transfer from to amount
  | amount <= 0 = return (from, to)
  | accountBalance from < amount = return (from, to)
  | otherwise = return ((from { accountBalance =  ((accountBalance from) - amount)}),(to { accountBalance =  ((accountBalance to) + amount)}))



-- END OF FOURTH COIN FLIP GAME WITH NOTES ******************************************************************************
-}




{-

-- START OF FIFTH COIN FLIP GAME WITH NOTES ******************************************************************************   
-- Sunday 10.01.21
-- Aim: Create Customer Data type, Create 10 Customers, Spawn 10 Customer Threads
-- to restructure the process so the interaction works properly

type Name = String
type AccountBalance = Int
data Account = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten deriving (Show, Eq)


data Customer = Customer {
  name :: Name,
  accountBalance :: AccountBalance,
  account :: Account
} deriving (Eq, Show)

-- first step : create a process that spawns a customer thread for each customer
customers :: Customer -> MVar Customer -> IO ()
customers customer_thread box = do 
   putMVar box customer_thread
   x <- randomAccountSelectors
   let payee = fst x
   let recipient = snd x
   let transferamount = randomN
   a <- transferamount
   putStrLn $ "Random Payee is: Account " ++ (show payee) ++ " || " ++ " Random Recipient is: Account " ++ (show recipient) ++ " || " ++ "Transfer amount is: " ++ (show a)
   -- transfer doesn't work as i'm selecting accounts 
   --(payee, recipient) <- transfer payee recipient 10 
   threadDelay 100
   customers customer_thread box

main :: IO ()
main = do
    
    box <- newEmptyMVar

     --CREATE 10 VALUES OF TYPE CUSTOMER
    putStrLn $ "Creating customers.."
    let c1 = Customer {name = "C1", accountBalance = 100, account = One}
    let c2 = Customer {name = "C2", accountBalance = 100, account = Two} 
    let c3 = Customer {name = "C3", accountBalance = 100, account = Three}
    let c4 = Customer {name = "C4", accountBalance = 20, account = Four}
    let c5 = Customer {name = "C5", accountBalance = 20, account = Five}
    let c6 = Customer {name = "C6", accountBalance = 20, account = Six}
    let c7 = Customer {name = "C3", accountBalance = 20, account = Seven}
    let c8 = Customer {name = "C8", accountBalance = 20, account= Eight}
    let c9 = Customer {name = "C9", accountBalance = 20, account = Nine}
    let c10 = Customer {name = "C10", accountBalance = 20, account = Ten}
    putStrLn $ "10 customers created."
    -- SPAWN A THREAD FOR EACH CUSTOMER

    putStrLn $ "Creating threads.."
    {- -- testing transfer between threads in MVars : does not work
    x <- newEmptyMVar
    let y = forkIO (customers c1 box)
    putMVar x y
    w <- takeMVar x

    a <- newEmptyMVar
    let b = forkIO (customers c2 box)
    putMVar a b
    c <- takeMVar b
    
    (c,w) <- transfer c w 10
    -}

    forkIO (customers c3 box)
    forkIO (customers c4 box)
    forkIO (customers c5 box)
    forkIO (customers c6 box)
    forkIO (customers c7 box)
    forkIO (customers c8 box)
    forkIO (customers c9 box)
    forkIO (customers c10 box)
    putStrLn $ "10 customer threads created." 
    

    a <- newEmptyMVar
    putMVar a c1
    b <- takeMVar a
    print b
    
    putStrLn $ "exit "

-- get any random number between 10 : 50 for the transfer
randomN :: IO Int 
randomN = do
    r <- randomRIO (10, 50)
    return r

randomAccountSelectors :: IO (Account, Account)
randomAccountSelectors = do
    n <- randomIO :: IO Int
    let random = mapIntToAccount n
    m <- randomIO :: IO Int
    let randomb = mapIntToAccount m
    if random == randomb then do
     randomAccountSelectors 
       else do return (random, randomb)
       

mapIntToAccount :: Int -> Account
mapIntToAccount  n = case r of
      0 -> One
      1 -> Two
      2 -> Three
      3 -> Four
      4 -> Five
      5 -> Six
      6 -> Seven
      7 -> Eight
      8 -> Nine
      9 -> Ten 
    where r = mod n 10


transfer :: Customer -> Customer -> Int -> IO (Customer, Customer)
transfer from to amount
  -- | amount <= 0 = return (from, to)
  | accountBalance from < amount = return (from, to)
  | otherwise = return ((from { accountBalance =  ((accountBalance from) - amount)}),(to { accountBalance =  ((accountBalance to) + amount)}))

-- I have :
-- Created 10 customers
-- Created 10 cusstomer threads
-- Selected two account numbers   (other way to select customers with coin flips )
-- Selected random amount to transfer
-- Can't trasfer money from Account , only by customer, so stuck


{- add this to process? And keep details in there
 MVar (Payee, Recipient, Amount)-}

-- END OF FIFTH COIN FLIP GAME WITH NOTES ******************************************************************************

 -}

 -- START OF SIXTH COIN FLIP GAME WITH NOTES ******************************************************************************   
-- Sunday 10.01.21
-- Aim: to restructure 4

data Coin = Head | Tail  deriving (Show, Eq)
type Name = String
type FirstWinner = String
type SecondWinner = String
type AccountBalance = Int
     
coinFlip :: IO Coin
coinFlip = do 
    r <- randomIO :: IO Bool
    return $ if r then Head else Tail


data Account = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten deriving (Show, Eq)

data Customer = Customer {
  name :: Name,
  accountBalance :: AccountBalance,
  account :: Account
} deriving (Eq, Show)

customerthread :: Customer -> MVar (Customer, FirstWinner) -> MVar SecondWinner -> MVar Coin -> IO ()
customerthread cust firstwinner secondwinner box = do 
    coin <- takeMVar box 
    putStrLn $ "Customer: " ++ (show cust) ++ " is playing. "
    putStrLn $ "test OG" ++ (show coin)
    coin1 <- coinFlip
    putStrLn $ "They got --- " ++ (show coin1)
    if coin1 == coin then do
        putStrLn $ "Woop! There was a match between the coin in the box and the coin the customer got - so Customer: " ++ (show cust) ++  " will get the transfer"
        putMVar firstwinner (cust, ("Customer: " ++ (show cust) ++ " gets the transfer..." ++ " Their customer account details are: " ++ (show cust) ))
        putStrLn $ "Now putting coin back in the box to find the payee - which is the next random Customer Thread to flip the same as the original coin"
        putMVar box coin1
        threadDelay 100
        putStrLn $ "Customer " ++ (show cust) ++ " is chosen to play next"
        coin1 <- coinFlip
        putStrLn $ "test NC" ++ (show coin1)
        else do
            putStrLn $ "No match this time - putting coin back in the box to try again"
            putMVar box coin
            threadDelay 100 
            customerthread cust firstwinner secondwinner box

main :: IO ()
main = do
    coin <- coinFlip
    putStrLn $ "ORIGINAL COIN IS: " ++ (show coin)

    box <- newMVar coin 

    firstwinner <- newEmptyMVar
    secondwinner <- newEmptyMVar

    let c1 = Customer {name = "C1", accountBalance = 100, account = One}
    let c2 = Customer {name = "C2", accountBalance = 100, account = Two} 
    let c3 = Customer {name = "C3", accountBalance = 100, account = Three}
    let c4 = Customer {name = "C4", accountBalance = 20, account = Four}
    let c5 = Customer {name = "C5", accountBalance = 20, account = Five}
    let c6 = Customer {name = "C6", accountBalance = 20, account = Six}
    let c7 = Customer {name = "C3", accountBalance = 20, account = Seven}
    let c8 = Customer {name = "C8", accountBalance = 20, account= Eight}
    let c9 = Customer {name = "C9", accountBalance = 20, account = Nine}
    let c10 = Customer {name = "C10", accountBalance = 20, account = Ten}

    forkIO (customerthread c1 firstwinner secondwinner box)
    forkIO (customerthread c2 firstwinner secondwinner box)
    forkIO (customerthread c3 firstwinner secondwinner box)
    forkIO (customerthread c4 firstwinner secondwinner box)
    forkIO (customerthread c5 firstwinner secondwinner box)
    forkIO (customerthread c6 firstwinner secondwinner box)
    forkIO (customerthread c7 firstwinner secondwinner box)
    forkIO (customerthread c8 firstwinner secondwinner box)
    forkIO (customerthread c9 firstwinner secondwinner box)
    forkIO (customerthread c10 firstwinner secondwinner box)
     

    w <- takeMVar firstwinner
    z <- takeMVar secondwinner

    amount <- randomN
    putStrLn $ "The winning customer gets: £" ++ (show amount)
    putStrLn $ "The first winner is: " ++ (show w)
    putStrLn $ "exit "



randomN :: IO Int 
randomN = do
    r <- randomRIO (10, 50)
    return r

transfer :: Customer -> Customer -> Int -> IO (Customer, Customer)
transfer from to amount
  | amount <= 0 = return (from, to)
  | accountBalance from < amount = return (from, to)
  | otherwise = return ((from { accountBalance =  ((accountBalance from) - amount)}),(to { accountBalance =  ((accountBalance to) + amount)}))



-- END OF SIXTH COIN FLIP GAME WITH NOTES ******************************************************************************

