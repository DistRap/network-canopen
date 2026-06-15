module Network.CANOpen
  ( module Network.CANOpen.API
  , module Network.CANOpen.Types
  , withCANOpen
  )
  where

import Control.Concurrent.Class.MonadSTM.TVar

import Control.Monad (forever, forM_, void, when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork (MonadFork)
import Control.Monad.Class.MonadTimer (MonadTimer)
import Control.Monad.Class.MonadThrow (MonadCatch, MonadMask)
import Control.Monad.Class.MonadSay (MonadSay (say))
import Control.Monad.Class.MonadSTM (MonadSTM(atomically))
import Data.Map (Map)
import Network.CAN (CANArbitrationField, CANMessage(..), CAN(..))
import Network.CANOpen.API
import Network.CANOpen.SDOClient
import Network.CANOpen.Types

import qualified Data.Map
import qualified Network.CANOpen.SDO
import qualified Network.CANOpen.NMT.Types

-- | Run CANOpen application
withCANOpen
  :: ( MonadAsync m
     , MonadCatch m
     , MonadFork m
     , MonadMask m
     , MonadSay m
     , MonadSTM m
     , MonadTimer m
     )
  => CAN m
  -> (CANOpen m -> m a)
  -> m a
withCANOpen can app = do
  handlersVar
    :: TVar m (Map CANArbitrationField (CANMessage -> m ()))
    <- newTVarIO mempty
  nodesVar
    :: TVar m (Map NodeID (CNode m))
    <- newTVarIO mempty

  -- incoming message router
  void $ async $ do
    forever $ do
      msg <- canRecv can
      handlers <- readTVarIO handlersVar
      forM_
        (Data.Map.toList handlers)
        (\(arb, handler) ->
          when
            (arb == canMessageArbitrationField msg)
            $ handler msg
        )
  let
    addNode nId = do
      removeNode nId
      sdoClient <- newSDOClient can canOpen nId

      let
        newNode =
          mkCNode
            nId
            sdoClient

      atomically
        $ modifyTVar
            nodesVar
            (Data.Map.insert nId newNode)

      registerHandler
        (Network.CANOpen.NMT.Types.nodeHeartbeatID nId)
        (say . show)

      pure newNode

    removeNode nId = do
      mNode <- atomically $ do
        nodes <- readTVar nodesVar
        case Data.Map.lookup nId nodes of
          Just n -> do
            modifyTVar nodesVar $ Data.Map.delete nId
            pure $ Just n
          Nothing -> pure Nothing

      case mNode of
        Nothing -> pure ()
        Just n -> do
          cNodeStopSDOClient n

          unregisterHandler
            (Network.CANOpen.SDO.sdoReplyID nId)
          unregisterHandler
            (Network.CANOpen.NMT.Types.nodeHeartbeatID nId)

    registerHandler cID handler = do
      atomically
        $ modifyTVar
            handlersVar
            $ Data.Map.insert cID handler

    unregisterHandler cID = do
      atomically
        $ modifyTVar
            handlersVar
            $ Data.Map.delete cID

    canOpen = CANOpen
        { canOpenAddNode = addNode
        , canOpenRemoveNode = removeNode
        , canOpenRegisterHandler = registerHandler
        , canOpenUnregisterHandler = unregisterHandler
        }

  app canOpen
