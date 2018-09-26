module Ex22_05 where

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

-- runReader ask 8 -- 8
