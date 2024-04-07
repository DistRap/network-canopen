-- | Network Management types
module Network.CANOpen.NMT.Types
  ( NMTState(..)
  , NMTMessage(..)
  , NMTCommand(..)
  , nodeHeartbeatID
  , nodeGuardingWord8ToNMTState
  , nmtID
  ) where

import Data.Word (Word8)
import Network.CAN (CANArbitrationField)
import Network.CANOpen.Types (NodeID(..))
import Network.CANOpen.Serialize (CSerialize(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import qualified Network.CAN
import qualified Test.QuickCheck

data NMTState
  = NMTState_Initialising
  | NMTState_Resetting
  | NMTState_ResettingComm
  | NMTState_PreOperational
  | NMTState_Operational
  | NMTState_Stopped
  deriving (Eq, Ord, Show)

data NMTCommand
  = NMTCommand_Start
  | NMTCommand_Stop
  | NMTCommand_PreOperation
  | NMTCommand_ResetNode
  | NMTCommand_ResetComm
  deriving (Eq, Ord, Show)

instance Arbitrary NMTCommand where
  arbitrary =
    Test.QuickCheck.oneof
    $ pure
    <$> [ NMTCommand_Start
        , NMTCommand_Stop
        , NMTCommand_PreOperation
        , NMTCommand_ResetNode
        , NMTCommand_ResetComm
        ]

instance CSerialize NMTCommand where
  put = put @Word8 . \case
    NMTCommand_Start        -> 0x01
    NMTCommand_Stop         -> 0x02
    NMTCommand_PreOperation -> 0x80
    NMTCommand_ResetNode    -> 0x81
    NMTCommand_ResetComm    -> 0x82

  get = get @Word8 >>= \case
    0x01 -> pure NMTCommand_Start
    0x02 -> pure NMTCommand_Stop
    0x80 -> pure NMTCommand_PreOperation
    0x81 -> pure NMTCommand_ResetNode
    0x82 -> pure NMTCommand_ResetComm
    x    -> fail $ "Invalid NMTCommand:" <> show x

-- | CAN ID used for heartbeat and node guarding messages
nodeHeartbeatID
  :: NodeID
  -> CANArbitrationField
nodeHeartbeatID (NodeID i) =
  Network.CAN.standardID (0x700 + fromIntegral i)

-- | Numeric state encoded in node guarding response
nodeGuardingWord8ToNMTState
  :: Word8
  -> Maybe NMTState
nodeGuardingWord8ToNMTState   4 = Just NMTState_Stopped
nodeGuardingWord8ToNMTState   5 = Just NMTState_Operational
nodeGuardingWord8ToNMTState 127 = Just NMTState_PreOperational
nodeGuardingWord8ToNMTState   _ = Nothing

-- | CAN ID used for NMT messages
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
  , nmtMessageCommand :: NMTCommand
  } deriving (Eq, Ord, Show)

instance Arbitrary NMTMessage where
  arbitrary = NMTMessage <$> arbitrary <*> arbitrary

instance CSerialize NMTMessage where
  put NMTMessage{..} = do
    put nmtMessageNodeID
    put nmtMessageCommand
  get = do
    nmtMessageNodeID <- get
    nmtMessageCommand <- get
    pure NMTMessage{..}
