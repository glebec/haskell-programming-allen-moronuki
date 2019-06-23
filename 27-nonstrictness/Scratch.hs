module Scratch where

import Debug.Trace (trace)

inc = (+1)

twice = inc . inc

howManyTimes' =
    let onePlusOne = trace "I got eval'd" (1 + 1)
    in inc onePlusOne + twice onePlusOne

-- evaluating howManyTimes' in GHCi shows only one trace, but not if typed
-- directly into GHCi – hm.
