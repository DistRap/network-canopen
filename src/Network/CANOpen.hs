{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.CANOpen where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Map (Map)
import Network.CAN (CANArbitrationField, CANMessage)
import Network.CANOpen.Types (NodeID)
import Network.CANOpen.NMT.Types (NMTState)

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM (TVar)

data Node = Node
  { nodeNMTState :: TVar NMTState
  --, nodeThreads :: [ThreadId]
  }

data CANOpenState m = CANOpenState
  { canOpenStateNodes :: Map NodeID Node
  , canOpenStateHandlers :: Map CANArbitrationField (CANMessage -> m ())
  }

class MonadCANOpen m where
  addNode
    :: NodeID
    -> m Node

setOperational = undefined

sdoWrite = undefined
sdoRead = undefined

data CANOpenError

newtype CANOpenT m a = CANOpenT
  { _unCANOpenT
      :: ExceptT CANOpenError
          (ReaderT (CANOpenState m) m) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (CANOpenState m)
    , MonadError CANOpenError
    , MonadIO
     )

instance MonadTrans CANOpenT where
  lift = CANOpenT . lift . lift

-- | Run CANOpenT transformer
runCANOpenT
  :: Monad m
  => CANOpenState m
  -> CANOpenT m a
  -> m (Either CANOpenError a)
runCANOpenT s =
    (`runReaderT` s)
  . runExceptT
  . _unCANOpenT
