module Network.CANOpen.Class where

import Network.CANOpen.Types (NodeID)
import Network.CAN (CANArbitrationField, CANMessage)

import Network.CANOpen.NMT.Types (NMTState(..))

data Node = Node
  { nodeID :: NodeID
  , nodeNMTState :: TVar NMTState
  , nodeSDOClient :: SDOClient
  }

class MonadCANOpen m where
  addNode
    :: NodeID
    -> m Node

  registerHandler
    :: CANArbitrationField
    -> (CANMessage -> IO ())
    -> m ()


