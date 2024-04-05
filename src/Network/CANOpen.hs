module Network.CANOpen where

import Data.Map (Map)
import Network.CANOpen.Types (NodeID)
import Network.CANOpen.NMT.Types (NMTState)

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM (TVar)

data Node = Node
  { nodeState :: TVar NMTState
  , nodeThreads :: [ThreadId]
  }

data CANOpenState = CANOpenState
  { canOpenStateNodes :: Map NodeID Node
  }

class MonadCANOpen m where
  addNode
    :: NodeID
    -> m Node

--waitBootup = undefined
setOperation = undefined

sdoWrite = undefined
sdoRead = undefined
