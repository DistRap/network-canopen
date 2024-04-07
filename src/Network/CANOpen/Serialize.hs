-- | CANOpen little endian serialization class
-- and instances

module Network.CANOpen.Serialize
  ( CSerialize(..)
  ) where

import Data.Serialize.Get (Get)
import Data.Serialize.Put (Putter)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)

import qualified Data.Serialize.Get
import qualified Data.Serialize.IEEE754
import qualified Data.Serialize.Put

class CSerialize a where
  put :: Putter a
  get :: Get a

-- * Bool as Word8

instance CSerialize Bool where
  put =
    put @Word8
    . \case
        False -> 0
        True -> 1
  get =
    get @Word8
    >>= \case
      0 -> pure False
      1 -> pure True
      x -> fail $ "Illegal bool encoding: " <> show x

-- * Unsigned

instance CSerialize Word8 where
  put = Data.Serialize.Put.putWord8
  get = Data.Serialize.Get.getWord8

instance CSerialize Word16 where
  put = Data.Serialize.Put.putWord16le
  get = Data.Serialize.Get.getWord16le

instance CSerialize Word32 where
  put = Data.Serialize.Put.putWord32le
  get = Data.Serialize.Get.getWord32le

instance CSerialize Word64 where
  put = Data.Serialize.Put.putWord64le
  get = Data.Serialize.Get.getWord64le

-- * Signed

instance CSerialize Int8 where
  put = Data.Serialize.Put.putInt8
  get = Data.Serialize.Get.getInt8

instance CSerialize Int16 where
  put = Data.Serialize.Put.putInt16le
  get = Data.Serialize.Get.getInt16le

instance CSerialize Int32 where
  put = Data.Serialize.Put.putInt32le
  get = Data.Serialize.Get.getInt32le

instance CSerialize Int64 where
  put = Data.Serialize.Put.putInt64le
  get = Data.Serialize.Get.getInt64le

-- * Floating

instance CSerialize Float where
  put = Data.Serialize.IEEE754.putFloat32le
  get = Data.Serialize.IEEE754.getFloat32le

instance CSerialize Double where
  put = Data.Serialize.IEEE754.putFloat64le
  get = Data.Serialize.IEEE754.getFloat64le
