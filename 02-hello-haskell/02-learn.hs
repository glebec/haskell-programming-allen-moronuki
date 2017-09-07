module Learn where

x = 10 * 5 + y
myResult = x * 5
y = 10

-- let a = 5
--     b = 7

foo x =
    let y = 6
        z = 2
    in 2 * y + z - x
