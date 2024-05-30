module Network.CANOpen.SDO where

import Data.Word (Word8)
import Network.CAN (MonadCAN(..), CANArbitrationField, CANMessage(..))
import Network.CANOpen.SDO.Types (SDOInit(..), SDORequest(..), SDOReply(..))
import Network.CANOpen.Types (NodeID(..), Mux(..))

import qualified Control.Monad
import qualified Network.CAN
import qualified Network.CANOpen.Serialize

data SDOClientCommand
  = SDOClientCommand_Upload NodeID Mux
  | SDOClientCommand_Download NodeID Mux [Word8]
  deriving (Eq, Show)

data SDOClientReply
  = SDOClientReply_Upload [Word8]
  | SDOClientReply_Download
  deriving (Eq, Show)

-- aka read (upload) from device
sdoClientUpload
  :: MonadCAN m
  => NodeID
  -> Mux
  -> m [Word8]
sdoClientUpload nID mux = do
  let
    sendReq = send . sdoRequest nID

  sendReq $ SDORequestUploadInit mux
  msg@CANMessage{..} <- recv
  case sdoReply nID msg of
    Left e -> error e
    Right SDOReplyUploadInit{..} -> do
      Control.Monad.unless
        (sdoReplyUploadInitMux == mux)
        $ error "MuxMismatch"

      let x@SDOInit{..} = sdoReplyUploadInitHeader
      case sdoInitExpedited of
        True ->
          case sdoInitSizeIndicated of
            True -> do
              let dataLength = 4 - (fromIntegral sdoInitNumBytes)
              pure
                $ take dataLength
                $ drop 4 canMessageData

            False -> error "Expedited but no size indicated"
        False -> error "TODO Segmented"
    Right _ -> error "Expected SDOReplyUploadInit"

-- aka write (download) to device
sdoClientDownload
  :: MonadCAN m
  => NodeID
  -> Mux
  -> [Word8]
  -> m ()
sdoClientDownload nID mux bytes = do
  let
    header =
      SDOInit
      { sdoInitNumBytes = 4 - (fromIntegral $ length bytes)
      , sdoInitExpedited = True
      , sdoInitSizeIndicated = True
      }

  send
    $ CANMessage
        (sdoRequestID nID)
        $ (Network.CANOpen.Serialize.runPut
            $ SDORequestDownloadInit header mux
          )
          ++ bytes

  sdoReply nID <$> recv
  >>= \case
    Left e -> error e
    Right SDOReplyDownloadInit{..} -> do
      Control.Monad.unless
        (sdoReplyDownloadInitMux == mux)
        $ error "MuxMismatch"

      pure ()
    Right _ -> error "Expected SDOReplyDownloadInit"

sdoRequest
  :: NodeID
  -> SDORequest
  -> CANMessage
sdoRequest nID req =
  CANMessage
    (sdoRequestID nID)
    $ Network.CANOpen.Serialize.pad 8
    $ Network.CANOpen.Serialize.runPut req

sdoReply
  :: NodeID
  -> CANMessage
  -> Either String SDOReply
sdoReply nID (CANMessage rxid dat) | rxid == sdoReplyID nID =
  Network.CANOpen.Serialize.runGet dat
sdoReply nID (CANMessage rxid _dat) | otherwise =
  Left
    $     "Expected SDO RX ID "
       <> show (sdoReplyID nID)
       <> ", but got "
       <> show rxid

sdoRequestID
  :: NodeID
  -> CANArbitrationField
sdoRequestID (NodeID i) =
  Network.CAN.standardID (0x600 + fromIntegral i)

sdoReplyID
  :: NodeID
  -> CANArbitrationField
sdoReplyID (NodeID i) =
  Network.CAN.standardID (0x580 + fromIntegral i)

