{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.CANOpen.Types
  ( NodeID(..)
  , NodeIdentity(..)
  -- * Dictionary
  , Index(..)
  , SubIndex(..)
  , Mux(..)
  -- ** Permission
  , Permission(..)
  -- ** Variable
  , Variable(..)
  ) where

import Data.Word (Word8, Word16, Word32)
import Network.CANOpen.Serialize (CSerialize(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import qualified Test.QuickCheck

newtype NodeID = NodeID
  { unNodeID :: Word8 }
  deriving (Eq, Ord, Show, Num)

instance Arbitrary NodeID where
  arbitrary = NodeID <$> Test.QuickCheck.choose (0, 128)

instance CSerialize NodeID where
  put = put . unNodeID
  get = NodeID <$> get

data NodeIdentity =
  NodeIdentity
  { nodeIdentityVendor :: Word32
  , nodeIdentityProduct :: Word32
  , nodeIdentityRevision :: Word32
  , nodeIdentitySerial :: Word32
  } deriving (Eq, Ord, Show)

instance Arbitrary NodeIdentity where
  arbitrary =
    NodeIdentity <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- * Dictionary

-- | Dictionary index
newtype Index = Index
  { unIndex :: Word16 }
  deriving (Eq, Ord, Show, Num)

instance Arbitrary Index where
  arbitrary = Index <$> arbitrary

instance CSerialize Index where
  put = put . unIndex
  get = Index <$> get

-- | Dictionary sub-index
newtype SubIndex = SubIndex
  { unSubIndex :: Word8 }
  deriving (Eq, Ord, Show, Num)

instance CSerialize SubIndex where
  put = put . unSubIndex
  get = SubIndex <$> get

instance Arbitrary SubIndex where
  arbitrary = SubIndex <$> arbitrary

-- | Full object dictionary address
data Mux =
  Mux
  { muxIndex :: Index
  , muxSubIndex :: SubIndex
  } deriving (Eq, Ord, Show)

instance Arbitrary Mux where
  arbitrary = Mux <$> arbitrary <*> arbitrary

instance CSerialize Mux where
  put m = put (muxIndex m) >> put (muxSubIndex m)
  get = Mux <$> get <*> get

-- | Variable access permissions
data Permission
  = Permission_Read
  | Permission_Write
  | Permission_ReadWrite
  | Permission_Const
  | Permission_Reserved
  deriving (Eq, Ord, Show)

-- | CANOpen variable
--
-- Phantom type carries its type,
-- while data type has fields for
-- its address (@Mux@), name..
data Variable a =
  Variable
    { variableName :: String
    , variableMux :: Mux
    , variablePerm :: Permission
    }
  deriving (Eq, Ord, Show)
