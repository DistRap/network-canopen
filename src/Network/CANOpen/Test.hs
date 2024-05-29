module Network.CANOpen.Test where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (MonadIO(liftIO))

--import Data.Default.Class
import Data.Word (Word8, Word32)
import Data.Int (Int32)

import Network.CAN
import Network.CANOpen
import Network.CANOpen.Serialize (CSerialize(..))
import Network.CANOpen.Types
import Network.CANOpen.LSS
import Network.CANOpen.LSS.Types
import Network.CANOpen.SDO
import qualified Network.SLCAN
import Network.SocketCAN
import qualified Network.CANOpen.Serialize
import qualified Network.CANOpen.SubBus

import qualified System.IO

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import qualified Data.Map
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
  , sdoClientCommand :: TMVar SDOClientCommand
  , sdoClientReply :: TMVar SDOClientReply
  }

sdoRead
  :: ( CSerialize a
     , MonadIO m
     )
  => TMVar SDOClientCommand
  -> TMVar SDOClientReply
  -> NodeID
  -> Variable a
  -> m a
sdoRead cmd reply nId var = do
  liftIO
    $ atomically
    $ writeTMVar
        cmd
        $ SDOClientCommand_Upload
            nId
            (variableMux var)

  rep <-
    liftIO
    $ atomically
    $ takeTMVar reply

  case rep of
    SDOClientReply_Download -> error "bogus reply"
    SDOClientReply_Upload bytes -> do
      pure
      $ either (error "Deserialize fail") id
      $ Network.CANOpen.Serialize.runGet bytes

sdoWrite
  :: ( CSerialize a
     , MonadIO m
     )
  => TMVar SDOClientCommand
  -> TMVar SDOClientReply
  -> NodeID
  -> Variable a
  -> a
  -> m ()
sdoWrite cmd reply nId var val = do
  liftIO
    $ atomically
    $ writeTMVar
        cmd
        $ SDOClientCommand_Download
            nId
            (variableMux var)
            (Network.CANOpen.Serialize.runPut val)

  rep <-
    liftIO
    $ atomically
    $ takeTMVar reply

  case rep of
    SDOClientReply_Download -> pure ()
    SDOClientReply_Upload bytes -> error "Bogus"

main :: IO ()
main = do
  let
    l :: (MonadIO m, Show a) => a -> m ()
    l = liftIO . print

    doLSS = not True -- False

  tb <- newEmptyTMVarIO
  sdoCmd :: TMVar SDOClientCommand <- newEmptyTMVarIO
  sdoRep :: TMVar SDOClientReply <- newEmptyTMVarIO

  UnliftIO.Async.async $ do
    forever $ do
      sdoRead
        sdoCmd
        sdoRep
        nID
        ioOutput
        >>= l

      threadDelay 1000000

      forM_ [0, 1, 2, 4, 8] $ \x -> do
        sdoWrite
          sdoCmd
          sdoRep
          nID
          ioOutput
          x
        threadDelay 1000000

  --System.IO.withFile "/dev/can4discouart" System.IO.ReadWriteMode $ \h -> Network.SLCAN.runSLCAN h def $ do
  runSocketCAN "vcan0" $ do
    runCANOpen $ do
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
          {--
          -- read vendor id
          raw <- sdoClientUpload nID (Mux 0x1018 1) 
          -- >>= l
          let
            des =
              either (error "DesFail") id
              $ Network.CANOpen.Serialize.runGet raw
          l (des :: Word32)
          --}

          forever $ do
            cmd <- liftIO . atomically . takeTMVar $ sdoCmd
            l cmd
            case cmd of
              SDOClientCommand_Upload nID mux -> do
                raw <- sdoClientUpload nID mux
                liftIO
                  . atomically
                  $ writeTMVar
                      sdoRep
                      $ SDOClientReply_Upload raw

              SDOClientCommand_Download nID mux bytes -> do
                raw <- sdoClientDownload nID mux bytes
                liftIO
                  . atomically
                  $ writeTMVar
                      sdoRep
                      $ SDOClientReply_Download

          {--
          -- at 0x606C $ field "velocity_actual" sint32 & ro
          sdoClientUpload @Int32 nID (Mux 0x606C 0) >>= l
          -- at 0x60FF $ field "target_velocity" sint32
          sdoClientDownload @Int32 nID (Mux 0x60FF 0) (123242)
          --}

      pure ()
  >>= print
