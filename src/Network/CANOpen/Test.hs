module Network.CANOpen.Test where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (MonadIO(liftIO))

--import Data.Default.Class
import Data.Word (Word8, Word32)
import Data.Int (Int32)

import Network.CANOpen
import Network.CANOpen.Class
import Network.CANOpen.LSS
import Network.CANOpen.SDOClient
import Network.CANOpen.Types
--import Network.CANOpen.LSS.Types
--import qualified Network.SLCAN
import Network.SocketCAN

--import qualified System.IO

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

main :: IO ()
main = do
  let
    l :: (MonadIO m, Show a) => a -> m ()
    l = liftIO . print

    doLSS = not True -- False

  {--
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
  --}

  --System.IO.withFile "/dev/can4discouart" System.IO.ReadWriteMode $ \h -> Network.SLCAN.runSLCAN h def $ do
  runSocketCAN "vcan0" $ do
    void $ runCANOpen $ do
      io <- addNode (NodeID 1)
      vcb <- addNode (NodeID 2)
      sdoRead
        io
        ioOutput
        >>= l

      sdoRead
        vcb
        ioOutput
        >>= l

      forM_ [0, 1, 2, 4, 8, 0] $ \x -> do
        sdoWrite
          io
          ioOutput
          x
        liftIO $ threadDelay 1000000

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
