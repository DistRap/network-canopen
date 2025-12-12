module Network.CANOpen.FixedSize
  ( FixedSize(..)
  ) where

import Data.Proxy (Proxy)
import Data.Word
import Data.Int

class FixedSize a where
  fixedSize :: Proxy a -> Word8

instance FixedSize Bool   where fixedSize _ = 1

instance FixedSize Word8  where fixedSize _ = 1
instance FixedSize Word16 where fixedSize _ = 2
instance FixedSize Word32 where fixedSize _ = 4
instance FixedSize Word64 where fixedSize _ = 8

instance FixedSize Int8   where fixedSize _ = 1
instance FixedSize Int16  where fixedSize _ = 2
instance FixedSize Int32  where fixedSize _ = 4
instance FixedSize Int64  where fixedSize _ = 8

instance FixedSize Float  where fixedSize _ = 2
instance FixedSize Double where fixedSize _ = 4
