module Ex24_11_6to9 where

import Data.Word
import Text.Trifecta

-- 6. IPv4

newtype IPAddress = IPAddress Word32 deriving (Eq, Ord, Show)

-- 7. IPv6

data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord, Show)

-- 8. Write Show instances which render the typical formats for IPs

-- 9. Write `ip4to6`
