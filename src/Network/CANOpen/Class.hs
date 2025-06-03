module Network.CANOpen.Class
  ( SDOClient(..)
  , Node(..)
  , MonadCANOpen(..)
  , MonadNode(..)
  , CANOpenException(..)
  )
  where

import Control.Concurrent.STM (TVar, TMVar)
import UnliftIO.Async (Async)
import UnliftIO.Exception (Exception)
import Network.CANOpen.SDO
import Network.CANOpen.Serialize (CSerialize)
import Network.CANOpen.Types (NodeID, Variable)
import Network.CAN (CANArbitrationField, CANMessage)
import Network.CAN.Class (MonadCAN)

import Network.CANOpen.NMT.Types (NMTState)

data SDOClient = SDOClient
  { sdoClientAsync :: Async ()
  , sdoClientUpload' :: TMVar SDOClientUpload
  , sdoClientUploadReply :: TMVar SDOClientUploadReply
  , sdoClientDownload' :: TMVar SDOClientDownload
  , sdoClientDownloadReply :: TMVar Bool
  }

data Node = Node
  { nodeID :: NodeID
  , nodeNMTState :: TVar NMTState
  , nodeSDOClient :: SDOClient
  }

instance Eq Node where
  (==) a b = nodeID a == nodeID b

instance Show Node where
  show = ("CANOpen node ID "++) . show . nodeID

class MonadCAN m => MonadCANOpen m where
  addNode
    :: NodeID
    -> m Node

  registerHandler
    :: CANArbitrationField
    -> (CANMessage -> IO ())
    -> m ()

class Monad m => MonadNode m where
  sdoRead
    :: CSerialize a
    => Variable a
    -> m a

  sdoWrite
    :: CSerialize a
    => Variable a
    -> a
    -> m ()

data CANOpenException
  = CANOpenException_SDOUploadTimeout NodeID
  | CANOpenException_SDODownloadTimeout NodeID
  deriving Show

instance Exception CANOpenException
