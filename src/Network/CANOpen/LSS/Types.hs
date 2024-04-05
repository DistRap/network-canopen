module Network.CANOpen.LSS.Types
  ( lssTXID
  , lssRXID
  , LSSCommandSpecifier(..)
  , lssCommandSpecifierToWord8
  , word8ToLSSCommandSpecifier
  ) where

import Data.Word (Word8)
import Network.CAN (CANArbitrationField)
import qualified Network.CAN

lssTXID :: CANArbitrationField
lssTXID = Network.CAN.standardID 0x7E5

lssRXID :: CANArbitrationField
lssRXID = Network.CAN.standardID 0x7E4

data LSSCommandSpecifier
  = LSSCommandSpecifier_SwitchModeGlobal
  | LSSCommandSpecifier_SwitchModeSelectiveVendor
  | LSSCommandSpecifier_SwitchModeSelectiveProduct
  | LSSCommandSpecifier_SwitchModeSelectiveRevision
  | LSSCommandSpecifier_SwitchModeSelectiveSerial
  | LSSCommandSpecifier_SwitchModeSelectiveReply
  | LSSCommandSpecifier_ConfigNodeID
  | LSSCommandSpecifier_ConfigBitTiming
  | LSSCommandSpecifier_ActivateBitTiming
  | LSSCommandSpecifier_StoreConfig
  | LSSCommandSpecifier_InquireVendor
  | LSSCommandSpecifier_InquireProduct
  | LSSCommandSpecifier_InquireRevision
  | LSSCommandSpecifier_InquireSerial
  | LSSCommandSpecifier_InquireNodeID
  | LSSCommandSpecifier_IdentifyRemoteSlavesVendor
  | LSSCommandSpecifier_IdentifyRemoteSlavesProduct
  | LSSCommandSpecifier_IdentifyRemoteSlavesRevisionLow
  | LSSCommandSpecifier_IdentifyRemoteSlavesRevisionHi
  | LSSCommandSpecifier_IdentifyRemoteSlavesSerialLow
  | LSSCommandSpecifier_IdentifyRemoteSlavesSerialHi
  | LSSCommandSpecifier_IdentifyRemoteSlavesReply
  | LSSCommandSpecifier_IdentifyNonConfigured
  | LSSCommandSpecifier_IdentifyNonConfiguredReply
  deriving (Eq, Ord, Show)

lssCommandSpecifierToWord8
  :: LSSCommandSpecifier
  -> Word8
lssCommandSpecifierToWord8 LSSCommandSpecifier_SwitchModeGlobal                = 0x04
lssCommandSpecifierToWord8 LSSCommandSpecifier_SwitchModeSelectiveVendor       = 0x40
lssCommandSpecifierToWord8 LSSCommandSpecifier_SwitchModeSelectiveProduct      = 0x41
lssCommandSpecifierToWord8 LSSCommandSpecifier_SwitchModeSelectiveRevision     = 0x42
lssCommandSpecifierToWord8 LSSCommandSpecifier_SwitchModeSelectiveSerial       = 0x43
lssCommandSpecifierToWord8 LSSCommandSpecifier_SwitchModeSelectiveReply        = 0x44
lssCommandSpecifierToWord8 LSSCommandSpecifier_ConfigNodeID                    = 0x11
lssCommandSpecifierToWord8 LSSCommandSpecifier_ConfigBitTiming                 = 0x13
lssCommandSpecifierToWord8 LSSCommandSpecifier_ActivateBitTiming               = 0x13
lssCommandSpecifierToWord8 LSSCommandSpecifier_StoreConfig                     = 0x17
lssCommandSpecifierToWord8 LSSCommandSpecifier_InquireVendor                   = 0x5A
lssCommandSpecifierToWord8 LSSCommandSpecifier_InquireProduct                  = 0x5B
lssCommandSpecifierToWord8 LSSCommandSpecifier_InquireRevision                 = 0x5C
lssCommandSpecifierToWord8 LSSCommandSpecifier_InquireSerial                   = 0x5D
lssCommandSpecifierToWord8 LSSCommandSpecifier_InquireNodeID                   = 0x5E
lssCommandSpecifierToWord8 LSSCommandSpecifier_IdentifyRemoteSlavesVendor      = 0x46
lssCommandSpecifierToWord8 LSSCommandSpecifier_IdentifyRemoteSlavesProduct     = 0x47
lssCommandSpecifierToWord8 LSSCommandSpecifier_IdentifyRemoteSlavesRevisionLow = 0x48
lssCommandSpecifierToWord8 LSSCommandSpecifier_IdentifyRemoteSlavesRevisionHi  = 0x49
lssCommandSpecifierToWord8 LSSCommandSpecifier_IdentifyRemoteSlavesSerialLow   = 0x4A
lssCommandSpecifierToWord8 LSSCommandSpecifier_IdentifyRemoteSlavesSerialHi    = 0x4B
lssCommandSpecifierToWord8 LSSCommandSpecifier_IdentifyRemoteSlavesReply       = 0x4F
lssCommandSpecifierToWord8 LSSCommandSpecifier_IdentifyNonConfigured           = 0x4C
lssCommandSpecifierToWord8 LSSCommandSpecifier_IdentifyNonConfiguredReply      = 0x50

word8ToLSSCommandSpecifier
  :: Word8
  -> Maybe LSSCommandSpecifier
word8ToLSSCommandSpecifier 0x04 = Just LSSCommandSpecifier_SwitchModeGlobal
word8ToLSSCommandSpecifier 0x40 = Just LSSCommandSpecifier_SwitchModeSelectiveVendor
word8ToLSSCommandSpecifier 0x41 = Just LSSCommandSpecifier_SwitchModeSelectiveProduct
word8ToLSSCommandSpecifier 0x42 = Just LSSCommandSpecifier_SwitchModeSelectiveRevision
word8ToLSSCommandSpecifier 0x43 = Just LSSCommandSpecifier_SwitchModeSelectiveSerial
word8ToLSSCommandSpecifier 0x44 = Just LSSCommandSpecifier_SwitchModeSelectiveReply
word8ToLSSCommandSpecifier 0x11 = Just LSSCommandSpecifier_ConfigNodeID
word8ToLSSCommandSpecifier 0x13 = Just LSSCommandSpecifier_ConfigBitTiming
word8ToLSSCommandSpecifier 0x15 = Just LSSCommandSpecifier_ActivateBitTiming
word8ToLSSCommandSpecifier 0x17 = Just LSSCommandSpecifier_StoreConfig
word8ToLSSCommandSpecifier 0x5A = Just LSSCommandSpecifier_InquireVendor
word8ToLSSCommandSpecifier 0x5B = Just LSSCommandSpecifier_InquireProduct
word8ToLSSCommandSpecifier 0x5C = Just LSSCommandSpecifier_InquireRevision
word8ToLSSCommandSpecifier 0x5D = Just LSSCommandSpecifier_InquireSerial
word8ToLSSCommandSpecifier 0x5E = Just LSSCommandSpecifier_InquireNodeID
word8ToLSSCommandSpecifier 0x46 = Just LSSCommandSpecifier_IdentifyRemoteSlavesVendor
word8ToLSSCommandSpecifier 0x47 = Just LSSCommandSpecifier_IdentifyRemoteSlavesProduct
word8ToLSSCommandSpecifier 0x48 = Just LSSCommandSpecifier_IdentifyRemoteSlavesRevisionLow
word8ToLSSCommandSpecifier 0x49 = Just LSSCommandSpecifier_IdentifyRemoteSlavesRevisionHi
word8ToLSSCommandSpecifier 0x4A = Just LSSCommandSpecifier_IdentifyRemoteSlavesSerialLow
word8ToLSSCommandSpecifier 0x4B = Just LSSCommandSpecifier_IdentifyRemoteSlavesSerialHi
word8ToLSSCommandSpecifier 0x4F = Just LSSCommandSpecifier_IdentifyRemoteSlavesReply
word8ToLSSCommandSpecifier 0x4C = Just LSSCommandSpecifier_IdentifyNonConfigured
word8ToLSSCommandSpecifier 0x50 = Just LSSCommandSpecifier_IdentifyNonConfiguredReply
word8ToLSSCommandSpecifier _    = Nothing
