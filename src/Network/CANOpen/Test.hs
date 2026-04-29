module Network.CANOpen.Test where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Word (Word8, Word32)
import Data.Int (Int32)

import Network.CANOpen
import Network.CANOpen.LSS
import Network.SocketCAN

nID :: NodeID
nID = NodeID 1

vendorID :: Variable Word32
vendorID = Variable
  { variableName = "VendorID"
  , variableMux = Mux 0x1018 1
  , variablePerm = Permission_Read
  }

readVendorID :: CNode m -> m Word32
readVendorID cn = cNodeSDORead cn vendorID

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

-- vcb vacuum gauge 3
gauge3enable :: Variable Bool
gauge3enable = Variable
  { variableName = "VacuumGauge3enable"
  , variableMux = Mux 0x7300 0
  , variablePerm = Permission_ReadWrite
  }

gauge3actual :: Variable Float
gauge3actual = Variable
  { variableName = "VacuumGauge3actual"
  , variableMux = Mux 0x7302 0
  , variablePerm = Permission_Read
  }

main :: IO ()
main = do
  let
    l :: (MonadIO m, Show a) => a -> m ()
    l = liftIO . print

    doLSS = not True -- False

  withSocketCAN (mkCANInterface "vcan0") $ \can ->
    withCANOpen can $ \CANOpen{..} -> do
      io <- canOpenAddNode (NodeID 1)
      vcb <- canOpenAddNode (NodeID 2)
      let
        outWrite :: CNode m -> Word8 -> m ()
        outWrite c = cNodeSDOWrite c ioOutput

      cNodeSDORead
        io
        ioOutput
        >>= l

      cNodeSDORead
        vcb
        ioOutput
        >>= l

      cNodeSDOWrite
        vcb
        gauge3enable
        True

      -- release some pressure
      forM_ [0, 3, 0] $ \x -> do
        outWrite vcb x
        liftIO $ threadDelay 2_000_000

      forM_ [0..1000] $ \(_ :: Int) -> do
        cNodeSDORead
          vcb
          gauge3actual
        >>= l

      cNodeSDOWrite
        vcb
        gauge3enable
        False

      outWrite vcb 0

      --forever $ do
      --  switchModeGlobal LSSMode_Operation
      --  recv >>= l

      --forever $
      --  inquireNodeID >>= l

      when doLSS $ do
        configNodeID can nID >>= l
        inquireNodeID can >>= l
        storeConfig can >>= l
      --canEndpointRecv can >>= l
