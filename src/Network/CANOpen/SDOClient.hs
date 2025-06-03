{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.CANOpen.SDOClient where

import Control.Monad
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

import Network.CAN
import Network.CANOpen.Class
import Network.CANOpen.Serialize (CSerialize(..))
import Network.CANOpen.Types
import Network.CANOpen.SDO
import qualified Network.CANOpen.Serialize
import qualified Network.CANOpen.SubBus

import Control.Concurrent.STM (atomically, orElse)
import Control.Concurrent.STM.TMVar
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Async
import qualified UnliftIO.Exception
import qualified UnliftIO.Timeout

-- | Read SDO variable
--
-- Sends SDOClientUpload, waits for reply
-- and returns deserialized value
sdoReadNode
  :: ( CSerialize a
     , MonadIO m
     )
  => Node
  -> Variable a
  -> m a
sdoReadNode node var = do
  liftIO
    $ atomically
    $ writeTMVar
        (sdoClientUpload' (nodeSDOClient node))
        $ SDOClientUpload
            { sdoClientUploadNodeID = nodeID node
            , sdoClientUploadMux = variableMux var
            }

  rep <-
    liftIO
    $ atomically
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
     , MonadIO m
     )
  => Node
  -> Variable a
  -> a
  -> m ()
sdoWriteNode node var val = do
  liftIO
    $ atomically
    $ writeTMVar
        (sdoClientDownload' (nodeSDOClient node))
        $ SDOClientDownload
            { sdoClientDownloadNodeID = nodeID node
            , sdoClientDownloadMux = variableMux var
            , sdoClientDownloadBytes = Network.CANOpen.Serialize.runPut val
            }
  void
    $ liftIO
    $ atomically
    $ takeTMVar
    $ sdoClientDownloadReply
    $ nodeSDOClient node

-- Variants using MonadReader

withNode
  :: Node
  -> ReaderT Node m a
  -> m a
withNode = flip runReaderT

sdoRead'
  :: ( CSerialize a
     , MonadIO m
     , MonadReader Node m
     )
  => Variable a
  -> m a
sdoRead' var = ask >>= \node -> sdoReadNode node var

sdoWrite'
  :: ( CSerialize a
     , MonadIO m
     , MonadReader Node m
     )
  => Variable a
  -> a
  -> m ()
sdoWrite' var val = ask >>= \node -> sdoWriteNode node var val

instance MonadIO m => MonadNode (ReaderT Node m) where
  sdoRead = sdoRead'
  sdoWrite = sdoWrite'

-- | Create and register a SDO client
-- for given @NodeID@
newSDOClient
  :: ( MonadIO m
     , MonadCAN m
     , MonadCANOpen m
     , MonadUnliftIO m
     )
  => NodeID
  -> m SDOClient
newSDOClient nID = do
  (subBusTMVar, sdoUp, sdoUpReply, sdoDown, sdoDownReply)
    <- liftIO
      $ (,,,,) <$> newEmptyTMVarIO
               <*> newEmptyTMVarIO
               <*> newEmptyTMVarIO
               <*> newEmptyTMVarIO
               <*> newEmptyTMVarIO

  registerHandler
    (sdoReplyID nID)
    (atomically . writeTMVar subBusTMVar)

  sdoClientAsync <-
    UnliftIO.Async.async $ do
      Network.CANOpen.SubBus.runSubBus subBusTMVar $ do
        forever $ do
          cmd <-
            liftIO
              . atomically
              $ (Left <$> readTMVar sdoUp)
                `orElse`
                (Right <$> readTMVar sdoDown)

          case cmd of
            Left SDOClientUpload{..} -> do
              UnliftIO.Timeout.timeout
                1_000_000
                $ sdoClientUpload
                    sdoClientUploadNodeID
                    sdoClientUploadMux
              >>= \case
                Nothing ->
                  UnliftIO.Exception.throwIO
                    $ CANOpenException_SDOUploadTimeout
                        sdoClientUploadNodeID
                Just raw ->
                  liftIO
                    . atomically
                    $ do
                        void
                          $ takeTMVar sdoUp
                        writeTMVar
                          sdoUpReply
                          $ SDOClientUploadReply raw

            Right SDOClientDownload{..} -> do
              UnliftIO.Timeout.timeout
                1_000_000
                $ sdoClientDownload
                    sdoClientDownloadNodeID
                    sdoClientDownloadMux
                    sdoClientDownloadBytes
              >>= \case
                Nothing ->
                  UnliftIO.Exception.throwIO
                    $ CANOpenException_SDODownloadTimeout
                        sdoClientDownloadNodeID
                _ -> do
                  liftIO
                    . atomically
                    $ do
                        _ <- takeTMVar sdoDown
                        writeTMVar
                          sdoDownReply
                          True

  UnliftIO.Async.link sdoClientAsync
  pure $
    SDOClient
    { sdoClientAsync = sdoClientAsync
    , sdoClientUpload' = sdoUp
    , sdoClientUploadReply = sdoUpReply
    , sdoClientDownload' = sdoDown
    , sdoClientDownloadReply = sdoDownReply
    }


