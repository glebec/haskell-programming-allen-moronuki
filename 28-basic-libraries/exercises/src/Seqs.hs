module Seqs where

import Criterion.Main
import qualified Data.Sequence as S

lists :: [Int]
lists = [1..100000]

seqs :: S.Seq Int
seqs = S.fromList [1..100000]

benchSeqs :: IO ()
benchSeqs = defaultMain
    [ bench "indexing list" $ whnf (!! 9001) lists -- 2.153 E -5 s
    , bench "indexing sequence" $ whnf (`S.index` 9001) seqs -- 1.198 E -7 s
    ]
