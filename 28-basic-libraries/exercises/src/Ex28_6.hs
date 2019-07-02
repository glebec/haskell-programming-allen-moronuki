module Ex28_6 where

import Criterion.Main (defaultMain, bench, whnf)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Foldable (foldr')

insertUnit :: Ord a => a -> M.Map a () -> M.Map a ()
insertUnit a = M.insert a ()

mapFromList' :: [Int] -> M.Map Int ()
mapFromList' = foldr' insertUnit M.empty

setFromList' :: [Int] -> S.Set Int
setFromList' = foldr' S.insert S.empty

ex28_6 :: IO ()
ex28_6 = defaultMain
    [ bench "map insert" $ whnf mapFromList' [1..9999] -- 3.562 ms
    , bench "set insert" $ whnf setFromList' [1..9999] -- 2.830 ms
    ]
