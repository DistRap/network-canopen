module Network.CANOpen.API
  ( CANOpen(..)
  , CNode(..)
  , cNodeSDOReadArray
  , cNodeSDOWriteArray
  , CANOpenException(..)
  )
  where

import Control.Monad.Class.MonadThrow (Exception)

import Network.CANOpen.Serialize (CSerialize)
import Network.CANOpen.Types (Array(..), NodeID, Variable)
import Network.CAN (CANArbitrationField, CANMessage)

import qualified Control.Monad

data CANOpen m = CANOpen
  { canOpenAddNode
      :: NodeID
      -> m (CNode m)
  , canOpenRemoveNode
      :: NodeID
      -> m ()
  , canOpenRegisterHandler
      :: CANArbitrationField
      -> (CANMessage -> m ())
      -> m ()
  , canOpenUnregisterHandler
      :: CANArbitrationField
      -> m ()
  }

data CNode m = CNode
  { cNodeId
      :: NodeID
  , cNodeSDORead
      :: forall a
       . CSerialize a
      => Variable a
      -> m a
  , cNodeSDOWrite
      :: forall a
       . CSerialize a
      => Variable a
      -> a
      -> m ()
  , cNodeStopSDOClient
      :: m ()
  }

instance Eq (CNode m) where
  (==) a b = cNodeId a == cNodeId b

instance Show (CNode m) where
  show = ("CANOpen node ID "++) . show . cNodeId

cNodeSDOReadArray
  :: ( CSerialize a
     , Monad m
     )
  => CNode m
  -> Array a
  -> m [a]
cNodeSDOReadArray CNode{..} Array{..} = do
  count <- cNodeSDORead arrayCount
  Control.Monad.forM
    [1..fromIntegral count]
    $ cNodeSDORead . arrayElem

cNodeSDOWriteArray
  :: ( CSerialize a
     , Monad m
     )
  => CNode m
  -> Array a
  -> [a]
  -> m ()
cNodeSDOWriteArray CNode{..} Array{..} items = do
  -- Clear mapping by writing 0 count
  cNodeSDOWrite
    arrayCount
    0

  -- Write entries
  Control.Monad.forM_
    (zip [1..] items)
    $ \(subIdx, item) ->
        cNodeSDOWrite
          (arrayElem subIdx)
          item

  -- Write new count
  cNodeSDOWrite
    arrayCount
    $ fromIntegral
    $ length items

data CANOpenException
  = CANOpenException_SDOUploadTimeout NodeID
  | CANOpenException_SDODownloadTimeout NodeID
  deriving Show

instance Exception CANOpenException
