module Ex28_10_Q (ex28_10_Q) where

import Criterion.Main (defaultMain, bench, whnf)
import System.Random (StdGen(..), mkStdGen, randoms)
-- import Control.Monad.Trans.State

-- Okasaki Queue

data Queue a =
    Queue { inbox  :: [a]
          , outbox :: [a]
          } deriving (Eq, Show)

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue inb outb) = Queue (x:inb) outb

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] []) = Nothing
dequeue (Queue inb (o:os)) = Just (o, Queue inb os)
dequeue (Queue inb []) = dequeue (Queue [] $ reverse inb)

-- dequeueAction :: StateT (Queue a) Maybe a
-- dequeueAction = StateT dequeue

-- List Queue

newtype LQ a = LQ [a] deriving (Eq, Show)

enqueueL :: a -> LQ a -> LQ a
enqueueL x (LQ xs) = LQ $ xs ++ [x]

dequeueL :: LQ a -> Maybe (a, LQ a)
dequeueL (LQ []) = Nothing
dequeueL (LQ (x:xs)) = Just (x, LQ xs)

-- Benchmarking

data Command = Enqueue | Dequeue deriving (Eq, Show)

commandStream :: [Command]
commandStream = toCommand <$> randoms (mkStdGen 237846) where
    toCommand True = Enqueue
    toCommand False = Dequeue

listQ :: Int -> LQ ()
listQ rounds = foldr go (LQ []) commands where
    commands = take rounds commandStream
    go Enqueue q = enqueueL () q
    go Dequeue q = case dequeueL q of
        Nothing -> q
        Just (_, q') -> q'

okasakiQ :: Int -> Queue ()
okasakiQ rounds = foldr go (Queue [] []) commands where
    commands = take rounds commandStream
    go Enqueue q = enqueue () q
    go Dequeue q = case dequeue q of
        Nothing -> q
        Just (_, q') -> q'

ex28_10_Q :: IO ()
ex28_10_Q =
    defaultMain [ bench "List Queue" $ whnf listQ 1000 -- 21.62 μs
                , bench "Okasaki Queue" $ whnf okasakiQ 1000 -- 6.093 μs
                ]
