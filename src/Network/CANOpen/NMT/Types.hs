{-# LANGUAGE RecordWildCards #-}

-- | Network Management types
module Network.CANOpen.NMT.Types
  ( NMTState(..)
  , NMTCommandSpecifier(..)
  , nmtCommandSpecifierToWord8
  , word8ToNMTCommandSpecifier
  , nodeHeartbeatID
  , nmtID
  , NMTMessage(..)
  ) where

import Data.Word (Word8)
import Network.CAN (CANArbitrationField)
import Network.CANOpen.Types (NodeID(..))
import Network.CANOpen.Serialize (CSerialize(..))
import qualified Network.CAN

data NMTState
  = NMTState_Initialising
  | NMTState_Resetting
  | NMTState_ResettingComm
  | NMTState_PreOperational
  | NMTState_Operational
  | NMTState_Stopped
  deriving (Eq, Ord, Show)

data NMTCommandSpecifier
  = NMTCommandSpecifier_Start
  | NMTCommandSpecifier_Stop
  | NMTCommandSpecifier_PreOperation
  | NMTCommandSpecifier_ResetNode
  | NMTCommandSpecifier_ResetComm
  deriving (Eq, Ord, Show)

nmtCommandSpecifierToWord8
  :: NMTCommandSpecifier
  -> Word8
nmtCommandSpecifierToWord8 NMTCommandSpecifier_Start        = 0x01
nmtCommandSpecifierToWord8 NMTCommandSpecifier_Stop         = 0x02
nmtCommandSpecifierToWord8 NMTCommandSpecifier_PreOperation = 0x80
nmtCommandSpecifierToWord8 NMTCommandSpecifier_ResetNode    = 0x81
nmtCommandSpecifierToWord8 NMTCommandSpecifier_ResetComm    = 0x82

word8ToNMTCommandSpecifier
  :: Word8
  -> Maybe NMTCommandSpecifier
word8ToNMTCommandSpecifier 0x01 = Just NMTCommandSpecifier_Start
word8ToNMTCommandSpecifier 0x02 = Just NMTCommandSpecifier_Stop
word8ToNMTCommandSpecifier 0x80 = Just NMTCommandSpecifier_PreOperation
word8ToNMTCommandSpecifier 0x81 = Just NMTCommandSpecifier_ResetNode
word8ToNMTCommandSpecifier 0x82 = Just NMTCommandSpecifier_ResetComm
word8ToNMTCommandSpecifier _    = Nothing

instance CSerialize NMTCommandSpecifier where
  put = put . nmtCommandSpecifierToWord8
  get =
    word8ToNMTCommandSpecifier <$> get
    >>=
    maybe
      (fail "Invalid NMTCommandSpecifier")
      pure

nodeHeartbeatID
  :: NodeID
  -> CANArbitrationField
nodeHeartbeatID (NodeID i) = Network.CAN.standardID (0x700 + fromIntegral i)

nmtID :: CANArbitrationField
nmtID = Network.CAN.standardID 0x0

-- | NMT message
--
-- ID0 byte 1 node ID
--     byte 0 command
--
-- example NMT messages:
--
-- # start node 01
-- cansend can0 000#0101
--
-- # reset node 01
-- cansend can0 000#8101
data NMTMessage = NMTMessage
  { nmtMessageNodeID :: NodeID
  , nmtMessageCommand :: NMTCommandSpecifier
  } deriving (Eq, Ord, Show)

instance CSerialize NMTMessage where
  put NMTMessage{..} = do
    put nmtMessageNodeID
    put nmtMessageCommand
  get = do
    nmtMessageNodeID <- get
    nmtMessageCommand <- get
    pure NMTMessage{..}
