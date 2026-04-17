module Network.CANOpen.SDO where

import Data.Word (Word8)
import Network.CAN (CANArbitrationField, CANMessage(..), CANEndpoint(..))
import Network.CANOpen.SDO.Types (SDOInit(..), SDORequest(..), SDOReply(..))
import Network.CANOpen.Types (NodeID(..), Mux(..))

import qualified Control.Monad
import qualified Network.CAN
import qualified Network.CANOpen.Serialize

data SDOClientUpload = SDOClientUpload
  { sdoClientUploadNodeID :: NodeID
  , sdoClientUploadMux :: Mux
  } deriving (Eq, Show)

newtype SDOClientUploadReply = SDOClientUploadReply
  { unSDOClientUploadReply :: [Word8]
  } deriving (Eq, Show)

data SDOClientDownload = SDOClientDownload
  { sdoClientDownloadNodeID :: NodeID
  , sdoClientDownloadMux :: Mux
  , sdoClientDownloadBytes :: [Word8]
  } deriving (Eq, Show)

-- aka read (upload) from device
sdoClientUpload
  :: Monad m
  => CANEndpoint m
  -> NodeID
  -> Mux
  -> m [Word8]
sdoClientUpload can nID mux = do
  let
    sendReq = canEndpointSend can . sdoRequest nID

  sendReq $ SDORequestUploadInit mux
  msg@CANMessage{..} <- canEndpointRecv can
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
                $ take dataLength
                $ drop 4 canMessageData

            False -> error "Expedited but no size indicated"
        False -> error "TODO Segmented"
    Right _ -> error "Expected SDOReplyUploadInit"

-- aka write (download) to device
sdoClientDownload
  :: Monad m
  => CANEndpoint m
  -> NodeID
  -> Mux
  -> [Word8]
  -> m ()
sdoClientDownload can nID mux bytes = do
  let
    header =
      SDOInit
      { sdoInitNumBytes = 4 - (fromIntegral $ length bytes)
      , sdoInitExpedited = True
      , sdoInitSizeIndicated = True
      }

  canEndpointSend
    can
    $ CANMessage
        (sdoRequestID nID)
        $ (Network.CANOpen.Serialize.runPut
            $ SDORequestDownloadInit header mux
          )
          ++ bytes

  sdoReply nID <$> canEndpointRecv can
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

