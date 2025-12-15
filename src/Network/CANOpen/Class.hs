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
import Network.CANOpen.Types (Array(..), NodeID, Variable)
import Network.CAN (CANArbitrationField, CANMessage)
import Network.CAN.Class (MonadCAN)

import Network.CANOpen.NMT.Types (NMTState)

import qualified Control.Monad

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

  sdoReadArray
    :: CSerialize a
    => Array a
    -> m [a]
  sdoReadArray Array{..} = do
    count <- sdoRead arrayCount
    Control.Monad.forM
      [1..fromIntegral count]
      $ sdoRead . arrayElem

  sdoWriteArray
    :: CSerialize a
    => Array a
    -> [a]
    -> m ()
  sdoWriteArray Array{..} items = do
    -- Clear mapping by writing 0 count
    sdoWrite
      arrayCount
      0

    -- Write entries
    Control.Monad.forM_
      (zip [1..] items)
      $ \(subIdx, item) ->
          sdoWrite
            (arrayElem subIdx)
            item

    -- Write new count
    sdoWrite
      arrayCount
      $ fromIntegral
      $ length items

data CANOpenException
  = CANOpenException_SDOUploadTimeout NodeID
  | CANOpenException_SDODownloadTimeout NodeID
  deriving Show

instance Exception CANOpenException
