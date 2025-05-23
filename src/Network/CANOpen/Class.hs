module Network.CANOpen.Class
  ( SDOClient(..)
  , Node(..)
  , MonadCANOpen(..)
  , MonadNode
  , CANOpenException(..)
  )
  where

import Control.Concurrent.STM (TVar, TMVar)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import UnliftIO.Async (Async)
import UnliftIO.Exception (Exception)
import Network.CANOpen.Types (NodeID)
import Network.CANOpen.SDO
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

type MonadNode m =
  ( MonadIO m
  , MonadReader Node m
  )

data CANOpenException
  = CANOpenException_SDOUploadTimeout NodeID
  | CANOpenException_SDODownloadTimeout NodeID
  deriving Show

instance Exception CANOpenException
