module Network.CANOpen.Class
  ( CANOpen(..)
  , SDOClient(..)
  , Node(..)
  , MonadNode(..)
  , CANOpenException(..)
  )
  where

import Control.Concurrent.Class.MonadSTM.TVar (TVar)
import Control.Concurrent.Class.MonadSTM.TMVar (TMVar)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow (Exception)

import Network.CANOpen.SDO
import Network.CANOpen.Serialize (CSerialize)
import Network.CANOpen.Types (Array(..), NodeID, Variable)
import Network.CAN (CANArbitrationField, CANMessage)

import Network.CANOpen.NMT.Types (NMTState)

data SDOClient m = SDOClient
  { sdoClientAsync         :: Async m ()
  , sdoClientUpload'       :: TMVar m SDOClientUpload
  , sdoClientUploadReply   :: TMVar m SDOClientUploadReply
  , sdoClientDownload'     :: TMVar m SDOClientDownload
  , sdoClientDownloadReply :: TMVar m Bool
  }

data Node m = Node
  { nodeID :: NodeID
  , nodeNMTState :: TVar m NMTState
  , nodeSDOClient :: SDOClient m
  }

instance Eq (Node m) where
  (==) a b = nodeID a == nodeID b

instance Show (Node m) where
  show = ("CANOpen node ID "++) . show . nodeID

data CANOpen m = CANOpen
  { canOpenAddNode
      :: NodeID
      -> m (Node m)
  , canOpenRegisterHandler
      :: CANArbitrationField
      -> (CANMessage -> m ())
      -> m ()
  }

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
