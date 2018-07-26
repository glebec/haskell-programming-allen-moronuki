module Listy where

newtype Listy a = Listy [a] deriving (Eq, Show)

-- -- error: Duplicate instance declarations
-- -- (above is the purpose of this demo, the issue with orphaned instances)

-- instance Monoid (Listy a) where
--     mempty = Listy []
--     mappend (Listy l) (Listy l') = Listy $ mappend l l'
