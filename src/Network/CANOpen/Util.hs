
module Network.CANOpen.Util
  ( showHex
  , formatHex
  ) where

import Data.Bits (FiniteBits)
import Text.Printf (PrintfArg, printf)

import qualified Data.Bits

-- | Format number using hexadecimal notation with leading 0x,
-- padded according to its bit size
showHex :: (PrintfArg t, FiniteBits t) => t -> String
showHex x =
  printf
    ("0x%0"
    ++ (show $ Data.Bits.finiteBitSize x `div` 4)
    ++ "X"
    )
    x

-- | Format number using hexadecimal notation with leading 0x
formatHex :: PrintfArg t => t -> String
formatHex = printf "0x%x"
