module Network.CANOpen.Test where

--import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (MonadIO(liftIO))

--import Data.Default.Class
import Data.Word (Word8, Word32)
import Data.Int (Int32)

import Network.CANOpen
import Network.CANOpen.Serialize (CSerialize(..))
import Network.CANOpen.Types
import Network.CANOpen.LSS
--import Network.CANOpen.LSS.Types
import Network.CANOpen.SDO
--import qualified Network.SLCAN
import Network.SocketCAN
import qualified Network.CANOpen.Serialize
import qualified Network.CANOpen.SubBus

--import qualified System.IO

import Control.Concurrent
import Control.Concurrent.STM (atomically, orElse)
import Control.Concurrent.STM.TMVar
import UnliftIO.Async (Async)
import qualified UnliftIO.Async

nID :: NodeID
nID = NodeID 1

vendorID :: Variable Word32
vendorID = Variable
  { variableName = "VendorID"
  , variableMux = Mux 0x1018 1
  , variablePerm = Permission_Read
  }

-- cia401 outputs
ioOutput :: Variable Word8
ioOutput = Variable
  { variableName = "ioOutput"
  , variableMux = Mux 0x6200 1
  , variablePerm = Permission_ReadWrite
  }

-- posix test vars
velocityActual :: Variable Int32
velocityActual = Variable
  { variableName = "velocityActual"
  , variableMux = Mux 0x606C 0
  , variablePerm = Permission_Read
  }

targetVelocity :: Variable Int32
targetVelocity = Variable
  { variableName = "targetVelocity"
  , variableMux = Mux 0x60FF 0
  , variablePerm = Permission_ReadWrite
  }

data SDOClient = SDOClient
  { sdoClientAsync :: Async ()
  , sdoClientUpload' :: TMVar SDOClientUpload
  , sdoClientUploadReply :: TMVar SDOClientUploadReply
  , sdoClientDownload' :: TMVar SDOClientDownload
  }

-- | Read SDO variable
--
-- Sends SDOClientUpload, waits for reply
-- and returns deserialized value
sdoRead
  :: ( CSerialize a
     , MonadIO m
     )
  => SDOClient
  -> NodeID
  -> Variable a
  -> m a
sdoRead client nId var = do
  liftIO
    $ atomically
    $ writeTMVar
        (sdoClientUpload' client)
        $ SDOClientUpload
            { sdoClientUploadNodeID = nId
            , sdoClientUploadMux = variableMux var
            }

  rep <-
    liftIO
    $ atomically
    $ takeTMVar
    $ sdoClientUploadReply client

  pure
    $ either (error "Deserialize fail") id
    $ Network.CANOpen.Serialize.runGet
    $ unSDOClientUploadReply rep

sdoWrite
  :: ( CSerialize a
     , MonadIO m
     )
  => SDOClient
  -> NodeID
  -> Variable a
  -> a
  -> m ()
sdoWrite client nId var val = do
  liftIO
    $ atomically
    $ writeTMVar
        (sdoClientDownload' client)
        $ SDOClientDownload
            { sdoClientDownloadNodeID = nId
            , sdoClientDownloadMux = variableMux var
            , sdoClientDownloadBytes = Network.CANOpen.Serialize.runPut val
            }

main :: IO ()
main = do
  let
    l :: (MonadIO m, Show a) => a -> m ()
    l = liftIO . print

    doLSS = not True -- False

  tb <- newEmptyTMVarIO
  sdoUp <- newEmptyTMVarIO
  sdoUpReply <- newEmptyTMVarIO
  sdoDown <- newEmptyTMVarIO
  --sdoRep :: TMVar SDOClientReply <- newEmptyTMVarIO

  let
    sdoClient =
      SDOClient
        { sdoClientAsync = undefined
        , sdoClientUpload' = sdoUp
        , sdoClientUploadReply = sdoUpReply
        , sdoClientDownload' = sdoDown
        }

  UnliftIO.Async.async $ do
    forever $ do
      sdoRead
        sdoClient
        nID
        ioOutput
        >>= l

      threadDelay 1000000

      forM_ [0, 1, 2, 4, 8] $ \x -> do
        sdoWrite
          sdoClient
          nID
          ioOutput
          x
        threadDelay 1000000

  --System.IO.withFile "/dev/can4discouart" System.IO.ReadWriteMode $ \h -> Network.SLCAN.runSLCAN h def $ do
  runSocketCAN "vcan0" $ do
    void $ runCANOpen $ do
      --forever $ do
      --  switchModeGlobal LSSMode_Operation
      --  recv >>= l

      --forever $
      --  inquireNodeID >>= l

      when doLSS $ do
        configNodeID nID >>= l
        inquireNodeID >>= l
        storeConfig >>= l
      --recv >>= l

      registerHandler
        (sdoReplyID nID)
        (atomically . writeTMVar tb)

      -- UnliftIO.Async.async $ do
      Network.CANOpen.SubBus.runSubBus tb $ do
          forever $ do
            cmd <-
              liftIO
                . atomically
                $ (Left <$> readTMVar (sdoClientUpload' sdoClient))
                  `orElse`
                  (Right <$> readTMVar (sdoClientDownload' sdoClient))

            l cmd
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
                        $ takeTMVar (sdoClientUpload' sdoClient)
                      writeTMVar
                        (sdoClientUploadReply sdoClient)
                        $ SDOClientUploadReply raw

              Right SDOClientDownload{..} -> do
                sdoClientDownload
                  sdoClientDownloadNodeID
                  sdoClientDownloadMux
                  sdoClientDownloadBytes
                void
                  . liftIO
                  . atomically
                  $ takeTMVar
                      (sdoClientDownload' sdoClient)
