
module Network.CANOpen.Test where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Default.Class
import Data.Word (Word32)
import Data.Int (Int32)

import Network.CAN
import Network.CANOpen
import Network.CANOpen.Types
import Network.CANOpen.LSS
import Network.CANOpen.LSS.Types
import Network.CANOpen.SDO
import qualified Network.SLCAN
import Network.SocketCAN

import qualified System.IO

nID :: NodeID
nID = NodeID 1

main :: IO ()
main = do
  let
    l :: (MonadIO m, Show a) => a -> m ()
    l = liftIO . print

    doLSS = True -- False

  cs <- newCANOpenState

  --System.IO.withFile "/tmp/ttyV0" System.IO.ReadWriteMode $ \h -> Network.SLCAN.runSLCAN h def $ do
  runSocketCAN "vcan0" $ do
    runCANOpenT cs $ do
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

      -- read vendor id
      sdoClientUpload @Word32 nID (Mux 0x1018 1)

      -- at 0x606C $ field "velocity_actual" sint32 & ro
      sdoClientUpload @Int32 nID (Mux 0x606C 0) >>= l
      -- at 0x60FF $ field "target_velocity" sint32
      sdoClientDownload @Int32 nID (Mux 0x60FF 0) (123242)

      forever $
        sdoClientUpload @Int32 nID (Mux 0x606C 0) >>= l
      --  sdoClientUpload @Word32 nID (Mux 0x606C 0)

      pure ()
  >>= print
