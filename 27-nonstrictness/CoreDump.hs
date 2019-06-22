module CoreDump where

discriminatory :: Bool -> Int
discriminatory b =
    case b of
        False -> 0
        True -> 1

{------------------------
> :set -ddump-simpl
> :set -dsuppress-all
> :l CoreDump.hs

discriminatory
discriminatory
  = \ b_a1yx ->
      case b_a1yx of {
        False -> I# 0#;
        True -> I# 1#
      }
-------------------------}

discriminatory2 :: Bool -> Int
discriminatory2 b =
    let x = undefined
    in case x `seq` b of
        False -> 0
        True -> 1

-- actual result in GHC 8.6.? was more complex, but basically:

{--------------------------

discriminatory2 =
    \ b_a10D ->
        let { x_a10E
            x_a10E = undefined } in
        case
            case x_a10E of _ {
                __DEFAULT -> b_a10D
            } of _ {
                False -> I# 0;
                True -> I# 1
            }

--------------------------}
