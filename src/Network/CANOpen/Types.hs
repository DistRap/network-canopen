{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.CANOpen.Types
  ( NodeID(..)
  -- * Dictionary
  , Index(..)
  , SubIndex(..)
  , Mux(..)
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

instance CSerialize Index where
  put = put . unIndex
  get = Index <$> get

newtype SubIndex = SubIndex
  { unSubIndex :: Word8 }
  deriving (Eq, Ord, Show, Num)

instance CSerialize SubIndex where
  put = put . unSubIndex
  get = SubIndex <$> get

data Mux =
  Mux
  { muxIndex :: Index
  , muxSubIndex :: SubIndex
  } deriving (Eq, Ord, Show)

instance CSerialize Mux where
  put m = put (muxIndex m) >> put (muxSubIndex m)
  get = Mux <$> get <*> get


