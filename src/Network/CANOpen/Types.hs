{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.CANOpen.Types
  ( NodeID(..)
  -- * Dictionary
  , Index(..)
  , SubIndex(..)
  , VariableAddress(..)
  ) where

import Data.Word (Word8, Word16)
import Network.CANOpen.Serialize (CSerialize(..))

newtype NodeID = NodeID
  { unNodeID :: Word8 }
  deriving (Eq, Ord, Show, Num)

instance CSerialize NodeID where
  put = put . unNodeID
  get = NodeID <$> get

-- * Dictionary

newtype Index = Index
  { unIndex :: Word16 }
  deriving (Eq, Ord, Show, Num)

newtype SubIndex = SubIndex
  { unSubIndex :: Word8 }
  deriving (Eq, Ord, Show, Num)

data VariableAddress a =
  Variable
  { variableAddressIndex :: Index
  , variableAddressSubIndex :: SubIndex
  }


