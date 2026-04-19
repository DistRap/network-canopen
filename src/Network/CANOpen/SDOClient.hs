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
import Network.CANOpen.API
import Network.CANOpen.Serialize (CSerialize(..))
import Network.CANOpen.Types
import Network.CANOpen.SDO
  ( SDOClientUpload(..)
  , SDOClientUploadReply(..)
  , SDOClientDownload(..)
  )
import qualified Network.CANOpen.SDO
import qualified Network.CANOpen.Serialize
import qualified Network.CANOpen.SubBus

data SDOClient m = SDOClient
  { sdoClientAsync         :: Async m ()
  -- ^ SDO Client thread
  , sdoClientUpload        :: TMVar m SDOClientUpload
  -- ^ Upload requests
  , sdoClientUploadReply   :: TMVar m SDOClientUploadReply
  -- ^ Upload replies
  , sdoClientDownload      :: TMVar m SDOClientDownload
  -- ^ Download requests
  , sdoClientDownloadReply :: TMVar m Bool
  -- ^ Download replies / acks
  }

-- | Read SDO variable
--
-- Sends SDOClientUpload, waits for reply
-- and returns deserialized value
sdoReadNode
  :: ( CSerialize a
     , MonadSTM m
     , MonadThrow m
     )
  => NodeID
  -> SDOClient m
  -> Variable a
  -> m a
sdoReadNode nodeId sdoClient var = do
  atomically
    $ putTMVar
        (sdoClientUpload sdoClient)
        $ SDOClientUpload
            { sdoClientUploadNodeID = nodeId
            , sdoClientUploadMux = variableMux var
            }

  rep <-
    atomically
    $ takeTMVar
    $ sdoClientUploadReply
    $ sdoClient

  pure
    $ either (error "Deserialize fail") id
    $ Network.CANOpen.Serialize.runGet
    $ unSDOClientUploadReply rep

-- | Write SDO variable
sdoWriteNode
  :: ( CSerialize a
     , MonadSTM m
     )
  => NodeID
  -> SDOClient m
  -> Variable a
  -> a
  -> m ()
sdoWriteNode nodeId sdoClient var val = do
  atomically
    $ putTMVar
        (sdoClientDownload sdoClient)
        $ SDOClientDownload
            { sdoClientDownloadNodeID = nodeId
            , sdoClientDownloadMux = variableMux var
            , sdoClientDownloadBytes = Network.CANOpen.Serialize.runPut val
            }
  void
    $ atomically
    $ takeTMVar
    $ sdoClientDownloadReply
    $ sdoClient

mkCNode
  :: ( MonadSTM m
     , MonadThrow m
     )
  => NodeID
  -> SDOClient m
  -> CNode m
mkCNode nodeId sdoClient =
  CNode
    { cNodeId = nodeId
    , cNodeSDORead = sdoReadNode nodeId sdoClient
    , cNodeSDOWrite = sdoWriteNode nodeId sdoClient
    }

sdoReadArray
  :: ( CSerialize a
     , Monad m
     )
  => CNode m
  -> Array a
  -> m [a]
sdoReadArray CNode{..} Array{..} = do
  count <- cNodeSDORead arrayCount
  Control.Monad.forM
    [1..fromIntegral count]
    $ cNodeSDORead . arrayElem

sdoWriteArray
  :: ( CSerialize a
     , Monad m
     )
  => CNode m
  -> Array a
  -> [a]
  -> m ()
sdoWriteArray CNode{..} Array{..} items = do
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
    (Network.CANOpen.SDO.sdoReplyID nID)
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
                $ Network.CANOpen.SDO.sdoClientUpload
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
                $ Network.CANOpen.SDO.sdoClientDownload
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
      { sdoClientAsync         = sdoClientAsync
      , sdoClientUpload        = sdoUp
      , sdoClientUploadReply   = sdoUpReply
      , sdoClientDownload      = sdoDown
      , sdoClientDownloadReply = sdoDownReply
      }
