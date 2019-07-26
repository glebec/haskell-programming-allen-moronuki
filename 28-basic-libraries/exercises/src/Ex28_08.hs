module Ex28_08 where

import Criterion.Main (defaultMain, bench, whnf)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

-- how much memory boxed and unboxed vectors containing the same data uses

source :: [Int]
source = [1..10000]

boxed :: V.Vector Int
boxed = {-# SCC "boxed" #-} V.fromList source

unboxed :: U.Vector Int
unboxed = {-# SCC "unboxed" #-} U.fromList source

ex28_08 :: IO ()
ex28_08 = defaultMain
    [ bench "boxed map" $ whnf (V.map (+1)) boxed -- 67.73 μs
    , bench "unboxed map" $ whnf (U.map (+1)) unboxed -- 10.00 μs
    ]
