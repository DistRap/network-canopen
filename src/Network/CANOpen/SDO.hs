module Network.CANOpen.SDO where

import Network.CAN (MonadCAN(..), CANArbitrationField, CANMessage(..))
--import Network.CANOpen.Serialize (CSerialize(..))
import Network.CANOpen.SDO.Types (SDOInit(..), SDORequest(..), SDOReply(..))
import Network.CANOpen.Serialize (CSerialize(..))
import Network.CANOpen.Types (NodeID(..), Mux(..))

import qualified Control.Monad
import qualified Network.CAN
import qualified Network.CANOpen.Serialize

-- aka read (upload) from device
sdoClientUpload
  :: ( CSerialize a
     , MonadCAN m
     )
  => NodeID
  -> Mux
  -> m a
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

      let SDOInit{..} = sdoReplyUploadInitHeader
      case sdoInitExpedited of
        True ->
          case sdoInitSizeIndicated of
            True -> do
              let dataLength = 4 - (fromIntegral sdoInitNumBytes)
              pure
                $ either (error "DesFail") id
                $ Network.CANOpen.Serialize.runGet
                $ take dataLength
                $ drop 4 canMessageData

            False -> error "Expedited but no size indicated"
        False -> error "TODO Segmented"
    Right _ -> error "Expected SDOReplyUploadInit"

-- aka write (download) to device
sdoClientDownload
  :: ( CSerialize a
     , MonadCAN m
     )
  => NodeID
  -> Mux
  -> a
  -> m ()
sdoClientDownload nID mux val = do
  let
    packedVal = Network.CANOpen.Serialize.runPut val
    header =
      SDOInit
      { sdoInitNumBytes = 4 - (fromIntegral $ length packedVal)
      , sdoInitExpedited = True
      , sdoInitSizeIndicated = True
      }

  send
    -- $ (\x -> x { canMessageData = canMessageData x ++ packedVal})
    $ CANMessage
        (sdoRequestID nID)
        $ (Network.CANOpen.Serialize.runPut
            $ SDORequestDownloadInit header mux
          )
          ++ packedVal

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

