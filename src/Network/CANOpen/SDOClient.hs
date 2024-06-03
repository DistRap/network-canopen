module Network.CANOpen.SDOClient where

import Control.Monad
import Control.Monad.IO.Class (MonadIO(liftIO))

import Network.CAN
import Network.CANOpen.Class
import Network.CANOpen.Serialize (CSerialize(..))
import Network.CANOpen.Types
import Network.CANOpen.SDO
import qualified Network.CANOpen.Serialize
import qualified Network.CANOpen.SubBus

import Control.Concurrent.STM (atomically, orElse)
import Control.Concurrent.STM.TMVar
--import UnliftIO.Async (Async)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Async

-- | Read SDO variable
--
-- Sends SDOClientUpload, waits for reply
-- and returns deserialized value
sdoRead
  :: ( CSerialize a
     , MonadIO m
     )
  => Node
  -> Variable a
  -> m a
sdoRead node var = do
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
sdoWrite
  :: ( CSerialize a
     , MonadIO m
     )
  => Node
  -> Variable a
  -> a
  -> m ()
sdoWrite node var val = do
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
              raw <-
                sdoClientUpload
                  sdoClientUploadNodeID
                  sdoClientUploadMux
              liftIO
                . atomically
                $ do
                    void
                      $ takeTMVar sdoUp
                    writeTMVar
                      sdoUpReply
                      $ SDOClientUploadReply raw

            Right SDOClientDownload{..} -> do
              sdoClientDownload
                sdoClientDownloadNodeID
                sdoClientDownloadMux
                sdoClientDownloadBytes
              liftIO
                . atomically
                $ do
                    _ <- takeTMVar sdoDown
                    writeTMVar
                      sdoDownReply
                      True

  pure $
    SDOClient
    { sdoClientAsync = sdoClientAsync
    , sdoClientUpload' = sdoUp
    , sdoClientUploadReply = sdoUpReply
    , sdoClientDownload' = sdoDown
    , sdoClientDownloadReply = sdoDownReply
    }


