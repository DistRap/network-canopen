module Network.CANOpen.API
  ( CANOpen(..)
  , CNode(..)
  , CANOpenException(..)
  )
  where

import Control.Monad.Class.MonadThrow (Exception)

import Network.CANOpen.Serialize (CSerialize)
import Network.CANOpen.Types (Array(..), NodeID, Variable)
import Network.CAN (CANArbitrationField, CANMessage)

data CANOpen m = CANOpen
  { canOpenAddNode
      :: NodeID
      -> m (CNode m)
  , canOpenRegisterHandler
      :: CANArbitrationField
      -> (CANMessage -> m ())
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
  }

instance Eq (CNode m) where
  (==) a b = cNodeId a == cNodeId b

instance Show (CNode m) where
  show = ("CANOpen node ID "++) . show . cNodeId

data CANOpenException
  = CANOpenException_SDOUploadTimeout NodeID
  | CANOpenException_SDODownloadTimeout NodeID
  deriving Show

instance Exception CANOpenException
