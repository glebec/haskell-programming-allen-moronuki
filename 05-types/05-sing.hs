module Sing where -- corrected from `sing` (lowercase)

fstString :: String -> String -- corrected from `++`
fstString x = x ++ " in the rain"

sndString :: String -> String -- corrected from `--> Char`
sndString x = x ++ " over the rainbow"

sing = if x < y then fstString x else sndString y -- corrected from `or`
    where x = "Singin"
          y = "Somewhere" -- corrected from x

-- all [Char] replaced with String to make HLint happy.
