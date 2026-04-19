{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.CANOpen.SDOClient where

import Control.Concurrent.Class.MonadSTM.TMVar
import Control.Monad
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork (MonadFork)
import Control.Monad.Class.MonadTimer (MonadTimer(timeout))
import Control.Monad.Class.MonadThrow (MonadMask, MonadThrow(throwIO))
import Control.Monad.Class.MonadSTM

import Network.CAN
import Network.CANOpen.Class
import Network.CANOpen.Serialize (CSerialize(..))
import Network.CANOpen.Types
import Network.CANOpen.SDO
import qualified Network.CANOpen.Serialize
import qualified Network.CANOpen.SubBus

-- | Read SDO variable
--
-- Sends SDOClientUpload, waits for reply
-- and returns deserialized value
sdoReadNode
  :: ( CSerialize a
     , MonadSTM m
     )
  => Node m
  -> Variable a
  -> m a
sdoReadNode node var = do
  atomically
    $ putTMVar
        (sdoClientUpload' (nodeSDOClient node))
        $ SDOClientUpload
            { sdoClientUploadNodeID = nodeID node
            , sdoClientUploadMux = variableMux var
            }

  rep <-
    atomically
    $ takeTMVar
    $ sdoClientUploadReply
    $ nodeSDOClient node

  pure
    $ either (error "Deserialize fail") id
    $ Network.CANOpen.Serialize.runGet
    $ unSDOClientUploadReply rep

-- | Write SDO variable
sdoWriteNode
  :: ( CSerialize a
     , MonadSTM m
     )
  => Node m
  -> Variable a
  -> a
  -> m ()
sdoWriteNode node var val = do
  atomically
    $ putTMVar
        (sdoClientDownload' (nodeSDOClient node))
        $ SDOClientDownload
            { sdoClientDownloadNodeID = nodeID node
            , sdoClientDownloadMux = variableMux var
            , sdoClientDownloadBytes = Network.CANOpen.Serialize.runPut val
            }
  void
    $ atomically
    $ takeTMVar
    $ sdoClientDownloadReply
    $ nodeSDOClient node

-- Variants using MonadReader

withCNode
  :: MonadSTM m
  => Node m
  -> (CNode m -> m a)
  -> m a
withCNode node act =
  act
    $ CNode
        { cNodeSDORead = sdoReadNode node
        , cNodeSDOWrite = sdoWriteNode node
        }

-- | Create and register a SDO client
-- for given @NodeID@
newSDOClient
  :: ( MonadAsync m
     , MonadFork m
     , MonadSTM m
     , MonadMask m
     , MonadTimer m
     , MonadThrow m
     )
  -- TODO: pass subBus to newSDOClient instead of passing main bus
  -- and building subBus here
  => CANEndpoint m
  -> CANOpen m
  -> NodeID
  -> m (SDOClient m)
newSDOClient can canOpen nID = do
  (subBusTMVar, sdoUp, sdoUpReply, sdoDown, sdoDownReply)
    <- atomically $
       (,,,,) <$> newEmptyTMVar
              <*> newEmptyTMVar
              <*> newEmptyTMVar
              <*> newEmptyTMVar
              <*> newEmptyTMVar

  canOpenRegisterHandler
    canOpen
    (sdoReplyID nID)
    (atomically . putTMVar subBusTMVar)

  sdoClientAsync <-
    async $ do
      Network.CANOpen.SubBus.withSubBus can subBusTMVar $ \subCan -> do
        forever $ do
          cmd <-
            atomically
              $ (Left <$> readTMVar sdoUp)
                `orElse`
                (Right <$> readTMVar sdoDown)

          case cmd of
            Left SDOClientUpload{..} -> do
              timeout
                1_000_000
                $ sdoClientUpload
                    subCan
                    sdoClientUploadNodeID
                    sdoClientUploadMux
              >>= \case
                Nothing ->
                  throwIO
                    $ CANOpenException_SDOUploadTimeout
                        sdoClientUploadNodeID
                Just raw ->
                  atomically
                    $ do
                        Control.Monad.void
                          $ takeTMVar sdoUp
                        writeTMVar
                          sdoUpReply
                          $ SDOClientUploadReply raw

            Right SDOClientDownload{..} -> do
              timeout
                1_000_000
                $ sdoClientDownload
                    subCan
                    sdoClientDownloadNodeID
                    sdoClientDownloadMux
                    sdoClientDownloadBytes
              >>= \case
                Nothing ->
                  throwIO
                    $ CANOpenException_SDODownloadTimeout
                        sdoClientDownloadNodeID
                _ -> do
                  atomically
                    $ do
                        _ <- takeTMVar sdoDown
                        writeTMVar
                          sdoDownReply
                          True

  link sdoClientAsync
  pure $
    SDOClient
    { sdoClientAsync = sdoClientAsync
    , sdoClientUpload' = sdoUp
    , sdoClientUploadReply = sdoUpReply
    , sdoClientDownload' = sdoDown
    , sdoClientDownloadReply = sdoDownReply
    }
{--
--}
