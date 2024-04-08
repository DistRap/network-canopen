-- | Link Layer Services types
module Network.CANOpen.LSS.Types
  ( LSSMode(..)
  , LSSRequest(..)
  , LSSReply(..)
  , LSSConfigNodeIDStatus(..)
  , LSSConfigBitTimingStatus(..)
  , LSSStoreConfigStatus(..)
  ) where

import Data.Word (Word8, Word16, Word32)
import Network.CANOpen.Types (NodeID)
import Network.CANOpen.Serialize (CSerialize(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import qualified Test.QuickCheck

data LSSMode
  = LSSMode_Operation
  | LSSMode_Configuration
  deriving (Eq, Ord, Show)

instance Arbitrary LSSMode where
  arbitrary =
    Test.QuickCheck.oneof
    $ pure
    <$> [ LSSMode_Operation
        , LSSMode_Configuration
        ]

instance CSerialize LSSMode where
  put = put @Word8 . \case
    LSSMode_Operation -> 0
    LSSMode_Configuration -> 1
  get = get @Word8 >>= \case
    0 -> pure $ LSSMode_Operation
    1 -> pure $ LSSMode_Configuration
    x -> fail $ "Invalid LSS switch mode argument" <> show x

data LSSRequest
  = LSSRequest_SwitchGlobal LSSMode
  | LSSRequest_SwitchSelectiveVendor Word32
  | LSSRequest_SwitchSelectiveProduct Word32
  | LSSRequest_SwitchSelectiveRevision Word32
  | LSSRequest_SwitchSelectiveSerial Word32
  | LSSRequest_ConfigNodeID NodeID
  | LSSRequest_ConfigBitTiming Word8 Word8
  -- ^ Table selector, table subindex
  | LSSRequest_ActivateBitTiming Word16
  -- ^ Switch delay of the two phases in milliseconds
  -- phase 1 until switch is done
  -- phase 2 until first message is sent after switching
  | LSSRequest_StoreConfig
  | LSSRequest_InquireVendor
  | LSSRequest_InquireProduct
  | LSSRequest_InquireRevision
  | LSSRequest_InquireSerial
  | LSSRequest_InquireNodeID
  | LSSRequest_IdentifyVendor Word32
  | LSSRequest_IdentifyProduct Word32
  | LSSRequest_IdentifyRevisionLow Word32
  | LSSRequest_IdentifyRevisionHi Word32
  | LSSRequest_IdentifySerialLow Word32
  | LSSRequest_IdentifySerialHi Word32
  | LSSRequest_IdentifyNonConfigured
  deriving (Eq, Ord, Show)

instance Arbitrary LSSRequest where
  arbitrary =
    Test.QuickCheck.oneof
    [ LSSRequest_SwitchGlobal <$> arbitrary
    , LSSRequest_SwitchSelectiveVendor <$> arbitrary
    , LSSRequest_SwitchSelectiveProduct <$> arbitrary
    , LSSRequest_SwitchSelectiveRevision <$> arbitrary
    , LSSRequest_SwitchSelectiveSerial <$> arbitrary
    , LSSRequest_ConfigNodeID <$> arbitrary
    , LSSRequest_ConfigBitTiming <$> arbitrary <*> arbitrary
    , LSSRequest_ActivateBitTiming <$> arbitrary
    , pure LSSRequest_StoreConfig
    , pure LSSRequest_InquireVendor
    , pure LSSRequest_InquireProduct
    , pure LSSRequest_InquireRevision
    , pure LSSRequest_InquireSerial
    , pure LSSRequest_InquireNodeID
    , LSSRequest_IdentifyVendor <$> arbitrary
    , LSSRequest_IdentifyProduct <$> arbitrary
    , LSSRequest_IdentifyRevisionLow <$> arbitrary
    , LSSRequest_IdentifyRevisionHi <$> arbitrary
    , LSSRequest_IdentifySerialLow <$> arbitrary
    , LSSRequest_IdentifySerialHi <$> arbitrary
    , pure LSSRequest_IdentifyNonConfigured
    ]

instance CSerialize LSSRequest where
  put = \case
    LSSRequest_SwitchGlobal m            -> put @Word8 0x04 >> put m
    LSSRequest_SwitchSelectiveVendor x   -> put @Word8 0x40 >> put x
    LSSRequest_SwitchSelectiveProduct x  -> put @Word8 0x41 >> put x
    LSSRequest_SwitchSelectiveRevision x -> put @Word8 0x42 >> put x
    LSSRequest_SwitchSelectiveSerial x   -> put @Word8 0x43 >> put x
    LSSRequest_ConfigNodeID x            -> put @Word8 0x11 >> put x
    LSSRequest_ConfigBitTiming ts ti     -> put @Word8 0x13 >> put ts >> put ti
    LSSRequest_ActivateBitTiming delay   -> put @Word8 0x15 >> put delay
    LSSRequest_StoreConfig               -> put @Word8 0x17
    LSSRequest_InquireVendor             -> put @Word8 0x5A
    LSSRequest_InquireProduct            -> put @Word8 0x5B
    LSSRequest_InquireRevision           -> put @Word8 0x5C
    LSSRequest_InquireSerial             -> put @Word8 0x5D
    LSSRequest_InquireNodeID             -> put @Word8 0x5E
    LSSRequest_IdentifyVendor x          -> put @Word8 0x46 >> put x
    LSSRequest_IdentifyProduct x         -> put @Word8 0x47 >> put x
    LSSRequest_IdentifyRevisionLow x     -> put @Word8 0x48 >> put x
    LSSRequest_IdentifyRevisionHi x      -> put @Word8 0x49 >> put x
    LSSRequest_IdentifySerialLow x       -> put @Word8 0x4A >> put x
    LSSRequest_IdentifySerialHi x        -> put @Word8 0x4B >> put x
    LSSRequest_IdentifyNonConfigured     -> put @Word8 0x4C

  get = get @Word8 >>= \case
    0x04 -> LSSRequest_SwitchGlobal <$> get
    0x40 -> LSSRequest_SwitchSelectiveVendor <$> get
    0x41 -> LSSRequest_SwitchSelectiveProduct <$> get
    0x42 -> LSSRequest_SwitchSelectiveRevision <$> get
    0x43 -> LSSRequest_SwitchSelectiveSerial <$> get
    0x11 -> LSSRequest_ConfigNodeID <$> get
    0x13 -> LSSRequest_ConfigBitTiming <$> get <*> get
    0x15 -> LSSRequest_ActivateBitTiming <$> get
    0x17 -> pure LSSRequest_StoreConfig
    0x5A -> pure LSSRequest_InquireVendor
    0x5B -> pure LSSRequest_InquireProduct
    0x5C -> pure LSSRequest_InquireRevision
    0x5D -> pure LSSRequest_InquireSerial
    0x5E -> pure LSSRequest_InquireNodeID
    0x46 -> LSSRequest_IdentifyVendor <$> get
    0x47 -> LSSRequest_IdentifyProduct <$> get
    0x48 -> LSSRequest_IdentifyRevisionLow <$> get
    0x49 -> LSSRequest_IdentifyRevisionHi <$> get
    0x4A -> LSSRequest_IdentifySerialLow <$> get
    0x4B -> LSSRequest_IdentifySerialHi <$> get
    0x4C -> pure LSSRequest_IdentifyNonConfigured
    x    -> fail $ "Invalid LSSRequest command specifier: " <> show x

data LSSConfigNodeIDStatus
  = LSSConfigNodeIDStatus_Ok
  | LSSConfigNodeIDStatus_ErrorOutOfRange
  | LSSConfigNodeIDStatus_ImplementationSpecificError Word8
  deriving (Eq, Ord, Show)

instance Arbitrary LSSConfigNodeIDStatus where
  arbitrary =
    Test.QuickCheck.oneof
    [ pure LSSConfigNodeIDStatus_Ok
    , pure LSSConfigNodeIDStatus_ErrorOutOfRange
    , LSSConfigNodeIDStatus_ImplementationSpecificError <$> arbitrary
    ]

instance CSerialize LSSConfigNodeIDStatus where
  put = \case
    LSSConfigNodeIDStatus_Ok -> put @Word8 0
    LSSConfigNodeIDStatus_ErrorOutOfRange -> put @Word8 1
    LSSConfigNodeIDStatus_ImplementationSpecificError specificErrorCode
      -> put @Word8 0xFF >> put @Word8 specificErrorCode
  get = get @Word8 >>= \case
    0    -> pure LSSConfigNodeIDStatus_Ok
    1    -> pure LSSConfigNodeIDStatus_ErrorOutOfRange
    0xFF -> LSSConfigNodeIDStatus_ImplementationSpecificError <$> get
    x    -> fail $ "Invalid LSSConfigNodeIDStatus: " <> show x

data LSSConfigBitTimingStatus
  = LSSConfigBitTimingStatus_Ok
  | LSSConfigBitTimingStatus_Unsupported
  | LSSConfigBitTimingStatus_ImplementationSpecificError Word8
  deriving (Eq, Ord, Show)

instance Arbitrary LSSConfigBitTimingStatus where
  arbitrary =
    Test.QuickCheck.oneof
    [ pure LSSConfigBitTimingStatus_Ok
    , pure LSSConfigBitTimingStatus_Unsupported
    , LSSConfigBitTimingStatus_ImplementationSpecificError <$> arbitrary
    ]

instance CSerialize LSSConfigBitTimingStatus where
  put = \case
    LSSConfigBitTimingStatus_Ok -> put @Word8 0
    LSSConfigBitTimingStatus_Unsupported -> put @Word8 1
    LSSConfigBitTimingStatus_ImplementationSpecificError specificErrorCode
      -> put @Word8 0xFF >> put @Word8 specificErrorCode
  get = get @Word8 >>= \case
    0    -> pure LSSConfigBitTimingStatus_Ok
    1    -> pure LSSConfigBitTimingStatus_Unsupported
    0xFF -> LSSConfigBitTimingStatus_ImplementationSpecificError <$> get
    x    -> fail $ "Invalid LSSConfigBitTimingStatus: " <> show x

data LSSStoreConfigStatus
  = LSSStoreConfigStatus_Ok
  | LSSStoreConfigStatus_StoringNotSupported
  | LSSStoreConfigStatus_MediaError
  | LSSStoreConfigStatus_ImplementationSpecificError Word8
  deriving (Eq, Ord, Show)

instance Arbitrary LSSStoreConfigStatus where
  arbitrary =
    Test.QuickCheck.oneof
    [ pure LSSStoreConfigStatus_Ok
    , pure LSSStoreConfigStatus_StoringNotSupported
    , pure LSSStoreConfigStatus_MediaError
    , LSSStoreConfigStatus_ImplementationSpecificError <$> arbitrary
    ]

instance CSerialize LSSStoreConfigStatus where
  put = \case
    LSSStoreConfigStatus_Ok -> put @Word8 0
    LSSStoreConfigStatus_StoringNotSupported -> put @Word8 1
    LSSStoreConfigStatus_MediaError -> put @Word8 2
    LSSStoreConfigStatus_ImplementationSpecificError specificErrorCode
      -> put @Word8 0xFF >> put @Word8 specificErrorCode
  get = get @Word8 >>= \case
    0    -> pure LSSStoreConfigStatus_Ok
    1    -> pure LSSStoreConfigStatus_StoringNotSupported
    2    -> pure LSSStoreConfigStatus_MediaError
    0xFF -> LSSStoreConfigStatus_ImplementationSpecificError <$> get
    x    -> fail $ "Invalid LSSStoreConfigStatus: " <> show x

data LSSReply
  = LSSReply_ConfigNodeID LSSConfigNodeIDStatus
  | LSSReply_ConfigBitTiming LSSConfigBitTimingStatus
  | LSSReply_StoreConfig LSSStoreConfigStatus
  | LSSReply_SwitchModeSelective
  | LSSReply_Identify
  | LSSReply_IdentifyNonConfigured
  | LSSReply_InquireVendor Word32
  | LSSReply_InquireProduct Word32
  | LSSReply_InquireRevision Word32
  | LSSReply_InquireSerial Word32
  | LSSReply_InquireNodeID NodeID
  deriving (Eq, Ord, Show)

instance Arbitrary LSSReply where
  arbitrary =
    Test.QuickCheck.oneof
    [ LSSReply_ConfigNodeID <$> arbitrary
    , LSSReply_ConfigBitTiming <$> arbitrary
    , LSSReply_StoreConfig <$> arbitrary
    , pure LSSReply_SwitchModeSelective
    , pure LSSReply_Identify
    , pure LSSReply_IdentifyNonConfigured
    , LSSReply_InquireVendor <$> arbitrary
    , LSSReply_InquireProduct <$> arbitrary
    , LSSReply_InquireRevision <$> arbitrary
    , LSSReply_InquireSerial <$> arbitrary
    , LSSReply_InquireNodeID <$> arbitrary
    ]

instance CSerialize LSSReply where
  put = \case
    LSSReply_ConfigNodeID x        -> put @Word8 0x11 >> put x
    LSSReply_ConfigBitTiming x     -> put @Word8 0x13 >> put x
    LSSReply_StoreConfig x         -> put @Word8 0x17 >> put x
    LSSReply_SwitchModeSelective   -> put @Word8 0x44
    LSSReply_Identify              -> put @Word8 0x4F
    LSSReply_IdentifyNonConfigured -> put @Word8 0x50
    LSSReply_InquireVendor x       -> put @Word8 0x5A >> put x
    LSSReply_InquireProduct x      -> put @Word8 0x5B >> put x
    LSSReply_InquireRevision x     -> put @Word8 0x5C >> put x
    LSSReply_InquireSerial x       -> put @Word8 0x5D >> put x
    LSSReply_InquireNodeID x       -> put @Word8 0x5E >> put x

  get = get @Word8 >>= \case
    0x11 -> LSSReply_ConfigNodeID <$> get
    0x13 -> LSSReply_ConfigBitTiming <$> get
    0x17 -> LSSReply_StoreConfig <$> get
    0x44 -> pure LSSReply_SwitchModeSelective
    0x4F -> pure LSSReply_Identify
    0x50 -> pure LSSReply_IdentifyNonConfigured
    0x5A -> LSSReply_InquireVendor <$> get
    0x5B -> LSSReply_InquireProduct <$> get
    0x5C -> LSSReply_InquireRevision <$> get
    0x5D -> LSSReply_InquireSerial <$> get
    0x5E -> LSSReply_InquireNodeID <$> get
    x    -> fail $ "Invalid LSSReply command specifier: " <> show x

-- TODO higher level api
-- maybe a class like hocd or GADTs
{--
 -
import Network.CANOpen.Types (NodeID, NodeIdentity)
data LSS =
  LSS_Switch LSSSwitchMode
  LSS_Identify LSSIdentify
  ...

data LSSSwitchMode
  = LSSSwitchMode_Global LSSMode
  -- ^ | Switch all devices
  | LSSSwitchMode_Selective NodeIdentity
  -- ^ | Switch specific device
  deriving (Eq, Ord, Show)

instance Arbitrary LSSSwitchMode where
  arbitrary =
    Test.QuickCheck.oneof
    [ LSSSwitchMode_Global <$> arbitrary
    , LSSSwitchMode_Selective <$> arbitrary
    ]

data LSSIdentify = LSSIdentify
  { lssIdentifyVendor :: Word32
  , lssIdentifyProduct :: Word32
  , lssIdentifyRevision :: (Word32, Word32) -- ^ (low, high)
  , lssIdentifySerial :: (Word32, Word32) -- ^ (low, high)
  } deriving (Eq, Ord, Show)

instance Arbitrary LSSIdentify where
  arbitrary =
    LSSIdentify <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--}


