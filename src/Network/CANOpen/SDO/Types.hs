module Network.CANOpen.SDO.Types
  ( SDOClientCommandSpecifier
  , SDOServerCommandSpecifier
  , SDOError(..)
  , sdoErrorToWord32
  , word32ToSDOError
  ) where

import Data.Word (Word32)

data SDOClientCommandSpecifier
  = SDOClientCommandSpecifier_DownloadSegment
  | SDOClientCommandSpecifier_DownloadInit
  | SDOClientCommandSpecifier_UploadInit
  | SDOClientCommandSpecifier_UploadSegment
  | SDOClientCommandSpecifier_Abort
  deriving (Bounded, Eq, Enum, Ord, Show)

data SDOServerCommandSpecifier
  = SDOServerCommandSpecifier_UploadSegment
  | SDOServerCommandSpecifier_DownloadSegment
  | SDOServerCommandSpecifier_UploadInit
  | SDOServerCommandSpecifier_DownloadInit
  | SDOServerCommandSpecifier_Abort
  deriving (Bounded, Eq, Enum, Ord, Show)

data SDOError
  = SDOError_ToggleNotAlternated
  -- ^ Toggle bit not alternated
  | SDOError_Timeout
  -- ^ SDO protocol timed out
  | SDOError_CommandSpecifierInvalid
  -- ^ Client/server command specifier not valid or unknown
  | SDOError_InvalidBlockSize
  -- ^ Invalid block size (block mode only)
  | SDOError_InvalidSequenceNumber
  -- ^ Invalid sequence number (block mode only)
  | SDOError_CRC
  -- ^ CRC error (block mode only)
  | SDOError_OOM
  -- ^ Out of memory
  | SDOError_UnsupportedAccess
  -- ^ Unsupported access to an object
  | SDOError_AttemptedWORead
  -- ^ Attempt to read write-only attribute
  | SDOError_AttemptedROWrite
  -- ^ Attempt to write read-only attribute
  | SDOError_NotFound
  -- ^ Object does not exist
  | SDOError_PDOMappingNotAllowed
  -- ^ Object cannot be mapped to the PDO.
  | SDOError_PDOMappingTooLarge
  -- ^ The number and length of the objects to be mapped would exceed PDO length
  | SDOError_GeneralParameterIncompatibility
  -- ^ General parameter incompatibility reason
  | SDOError_GeneralInternalIncompatibility
  -- ^ General internal incompatibility in the device
  | SDOError_HWError
  -- ^ Access failed due to an hardware error
  | SDOError_SizeMismatch
  -- ^ Data type does not match, length of service parameter does not match
  | SDOError_SizeMismatchParamHigh
  -- ^ Data type does not match, length of service parameter too high
  | SDOError_SizeMismatchParamLow
  -- ^ Data type does not match, length of service parameter too low
  | SDOError_SubindexNotFound
  -- ^ Sub-index does not exist
  | SDOError_ValueRangeExceeded
  -- ^ Value range of parameter exceeded (only for write access).
  | SDOError_ValueTooHigh
  -- ^ Value of parameter written too high
  | SDOError_ValueTooLow
  -- ^ Value of parameter written too low
  | SDOError_MaxLessThanMin
  -- ^ Maximum value is less than minimum value
  | SDOError_GeneralError
  -- ^ General error
  | SDOError_CannotTransferOrStore
  -- ^ Data cannot be transferred or stored to the application
  | SDOError_CannotTransferOrStoreLocalControl
  -- ^ Data cannot be transferred or stored to the application
  -- beacuse of local control
  | SDOError_CannotTransferOrStoreDevState
  -- ^ Data cannot be transferred or stored to the application
  -- beacuse of the present device state
  | SDOError_ObjectDictionaryDynamic
  -- ^ Object dictionary dynamic generation fails or no object dictionary is
  -- present (e.g. object dictionary is generated from file and generation fails
  -- because of an file error).
  deriving (Eq, Ord, Show)

sdoErrorToWord32
  :: SDOError
  -> Word32
sdoErrorToWord32 SDOError_ToggleNotAlternated               = 0x05030000
sdoErrorToWord32 SDOError_Timeout                           = 0x05040000
sdoErrorToWord32 SDOError_CommandSpecifierInvalid           = 0x05040001
sdoErrorToWord32 SDOError_InvalidBlockSize                  = 0x05040002
sdoErrorToWord32 SDOError_InvalidSequenceNumber             = 0x05040003
sdoErrorToWord32 SDOError_CRC                               = 0x05040004
sdoErrorToWord32 SDOError_OOM                               = 0x05040005
sdoErrorToWord32 SDOError_UnsupportedAccess                 = 0x06010000
sdoErrorToWord32 SDOError_AttemptedWORead                   = 0x06010001
sdoErrorToWord32 SDOError_AttemptedROWrite                  = 0x06010002
sdoErrorToWord32 SDOError_NotFound                          = 0x06020000
sdoErrorToWord32 SDOError_PDOMappingNotAllowed              = 0x06040041
sdoErrorToWord32 SDOError_PDOMappingTooLarge                = 0x06040042
sdoErrorToWord32 SDOError_GeneralParameterIncompatibility   = 0x06040043
sdoErrorToWord32 SDOError_GeneralInternalIncompatibility    = 0x06040047
sdoErrorToWord32 SDOError_HWError                           = 0x06060000
sdoErrorToWord32 SDOError_SizeMismatch                      = 0x06070010
sdoErrorToWord32 SDOError_SizeMismatchParamHigh             = 0x06070012
sdoErrorToWord32 SDOError_SizeMismatchParamLow              = 0x06070013
sdoErrorToWord32 SDOError_SubindexNotFound                  = 0x06090011
sdoErrorToWord32 SDOError_ValueRangeExceeded                = 0x06090030
sdoErrorToWord32 SDOError_ValueTooHigh                      = 0x06090031
sdoErrorToWord32 SDOError_ValueTooLow                       = 0x06090032
sdoErrorToWord32 SDOError_MaxLessThanMin                    = 0x06090036
sdoErrorToWord32 SDOError_GeneralError                      = 0x08000000
sdoErrorToWord32 SDOError_CannotTransferOrStore             = 0x08000020
sdoErrorToWord32 SDOError_CannotTransferOrStoreLocalControl = 0x08000021
sdoErrorToWord32 SDOError_CannotTransferOrStoreDevState     = 0x08000022
sdoErrorToWord32 SDOError_ObjectDictionaryDynamic           = 0x08000023

word32ToSDOError
  :: Word32
  -> SDOError
word32ToSDOError 0x05030000 = SDOError_ToggleNotAlternated
word32ToSDOError 0x05040000 = SDOError_Timeout
word32ToSDOError 0x05040001 = SDOError_CommandSpecifierInvalid
word32ToSDOError 0x05040002 = SDOError_InvalidBlockSize
word32ToSDOError 0x05040003 = SDOError_InvalidSequenceNumber
word32ToSDOError 0x05040004 = SDOError_CRC
word32ToSDOError 0x05040005 = SDOError_OOM
word32ToSDOError 0x06010000 = SDOError_UnsupportedAccess
word32ToSDOError 0x06010001 = SDOError_AttemptedWORead
word32ToSDOError 0x06010002 = SDOError_AttemptedROWrite
word32ToSDOError 0x06020000 = SDOError_NotFound
word32ToSDOError 0x06040041 = SDOError_PDOMappingNotAllowed
word32ToSDOError 0x06040042 = SDOError_PDOMappingTooLarge
word32ToSDOError 0x06040043 = SDOError_GeneralParameterIncompatibility
word32ToSDOError 0x06040047 = SDOError_GeneralInternalIncompatibility
word32ToSDOError 0x06060000 = SDOError_HWError
word32ToSDOError 0x06070010 = SDOError_SizeMismatch
word32ToSDOError 0x06070012 = SDOError_SizeMismatchParamHigh
word32ToSDOError 0x06070013 = SDOError_SizeMismatchParamLow
word32ToSDOError 0x06090011 = SDOError_SubindexNotFound
word32ToSDOError 0x06090030 = SDOError_ValueRangeExceeded
word32ToSDOError 0x06090031 = SDOError_ValueTooHigh
word32ToSDOError 0x06090032 = SDOError_ValueTooLow
word32ToSDOError 0x06090036 = SDOError_MaxLessThanMin
word32ToSDOError 0x08000000 = SDOError_GeneralError
word32ToSDOError 0x08000020 = SDOError_CannotTransferOrStore
word32ToSDOError 0x08000021 = SDOError_CannotTransferOrStoreLocalControl
word32ToSDOError 0x08000022 = SDOError_CannotTransferOrStoreDevState
word32ToSDOError 0x08000023 = SDOError_ObjectDictionaryDynamic
word32ToSDOError x          = error $ "Unknown SDO error code: " <> show x
