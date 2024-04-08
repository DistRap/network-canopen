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
import Network.CAN.Class
import Network.CANOpen.Types (NodeID)
import Network.CANOpen.NMT.Types (NMTState(..))

--import Control.Concurrent (ThreadId)
import Control.Concurrent.STM -- (TVar, TQueue)

import qualified Data.Map
import qualified Network.CANOpen.NMT.Types

data Node = Node
  { nodeNMTState :: TVar NMTState
  --, nodeThreads :: [ThreadId]
  }

data CANOpenState = CANOpenState
  { canOpenStateNodes :: TVar (Map NodeID Node)
  , canOpenStateHandlers :: TVar (Map CANArbitrationField (CANMessage -> IO ()))
  }

newCANOpenState
  :: IO CANOpenState
newCANOpenState = do
  nodes <- newTVarIO mempty
  handlers <- newTVarIO mempty
  pure
    CANOpenState
    { canOpenStateNodes = nodes
    , canOpenStateHandlers = handlers
    }

data CANOpenError = CANOpenError_Whatever
  deriving Show

newtype CANOpenT m a = CANOpenT
  { _unCANOpenT
      :: ExceptT CANOpenError
          (ReaderT CANOpenState m) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadCAN
    , MonadReader CANOpenState
    , MonadError CANOpenError
    , MonadIO
    )

instance MonadTrans CANOpenT where
  lift = CANOpenT . lift . lift

-- | Run CANOpenT transformer
runCANOpenT
  :: Monad m
  => CANOpenState
  -> CANOpenT m a
  -> m (Either CANOpenError a)
runCANOpenT s =
    (`runReaderT` s)
  . runExceptT
  . _unCANOpenT

class MonadCANOpen m where
  addNode
    :: NodeID
    -> m Node

  registerHandler
    :: CANArbitrationField
    -> (CANMessage -> IO ())
    -> m ()

--setOperational = undefined
--sdoWrite = undefined
--sdoRead = undefined

instance MonadIO m => MonadCANOpen (CANOpenT m) where
  addNode nID = do
    nState <- liftIO $ newTVarIO NMTState_Initialising
    let
      newNode =
        Node
        { nodeNMTState = nState
        }

    ts <- ask
    liftIO
      $ atomically
      $ modifyTVar
          (canOpenStateNodes ts)
          (Data.Map.insert nID newNode)

    registerHandler
      (Network.CANOpen.NMT.Types.nodeHeartbeatID nID)
      (liftIO . print)

    pure newNode

  registerHandler cID handler = do
    ts <- ask
    liftIO
      $ atomically
      $ modifyTVar
          (canOpenStateHandlers ts)
          (Data.Map.insert cID handler)
