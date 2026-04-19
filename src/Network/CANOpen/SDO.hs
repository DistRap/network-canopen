module Network.CANOpen.SDO where

import Control.Monad.Class.MonadThrow (Exception(..), MonadThrow(throwIO))
import Data.Word (Word8)
import Network.CAN (CANArbitrationField, CANMessage(..), CAN(..))
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

data SDOException
  = SDOException_Read SDOExceptionDetail
  | SDOException_Write SDOExceptionDetail
  deriving (Eq, Show)

data SDOExceptionDetail
  = SDOExceptionDetail_SDOReplyBadID CANArbitrationField CANArbitrationField
  | SDOExceptionDetail_SDOReplyDeserialize String
  | SDOExceptionDetail_MuxMismatch Mux Mux
  | SDOExceptionDetail_UnexpectedSDOReply String SDOReply
  | SDOExceptionDetail_GetBytesNonExpedited String
  | SDOExceptionDetail_VariableLengthDataDeserialize String String String
  | SDOExceptionDetail_VariableDataDeserialize String String String
  | SDOExceptionDetail_ExpeditedNoSizeIndicated
  | SDOExceptionDetail_UnsupportedSegmentedTransfer
    deriving (Eq, Show)

instance Exception SDOException where
  displayException = \case
    SDOException_Read d -> "sdoReadNode: " <> displayDetail d
    SDOException_Write d -> "sdoWriteNode: " <> displayDetail d

    where
      displayDetail = \case
        SDOExceptionDetail_SDOReplyBadID expected actual ->
          "Unexpected CAN ID of SDOReply, expecting " <> show expected <> " but got " <> show actual
        SDOExceptionDetail_SDOReplyDeserialize e ->
          "Can't deserialize SDOReply " <> e
        SDOExceptionDetail_MuxMismatch expected actual ->
          "Mux mismatch, expected " <> show expected <> " but got " <> show actual
        SDOExceptionDetail_UnexpectedSDOReply expected actual ->
          "Unexpected SDOReply, expecting " <> expected <> " but got " <> show actual
        SDOExceptionDetail_GetBytesNonExpedited e ->
          "Unable to getBytes following size (non-expedited transfer) " <> e
        SDOExceptionDetail_VariableLengthDataDeserialize e var raw ->
          "Unable to deserialize length-variable data "
           <> e <> " while reading " <> var <> " from bytes " <> raw
        SDOExceptionDetail_VariableDataDeserialize e var raw ->
          "Unable to deserialize variable data "
           <> e <> " while reading " <> var <> " from bytes " <> raw
        SDOExceptionDetail_ExpeditedNoSizeIndicated ->
          "Expedited transfer but no size indicated"
        SDOExceptionDetail_UnsupportedSegmentedTransfer ->
          "Segmented transfer not supported"

-- aka read (upload) from device
sdoClientUpload
  :: MonadThrow m
  => CAN m
  -> NodeID
  -> Mux
  -> m [Word8]
sdoClientUpload can nID mux = do
  canSend can
    $ CANMessage
        (sdoRequestID nID)
        $ Network.CANOpen.Serialize.pad 8
        $ Network.CANOpen.Serialize.runPut
        $ SDORequestUploadInit mux

  msg <- canRecv can

  Control.Monad.unless
    (canMessageArbitrationField msg == sdoReplyID nID)
    $ throw
        $ SDOExceptionDetail_SDOReplyBadID
            (sdoReplyID nID)
            (canMessageArbitrationField msg)

  case Network.CANOpen.Serialize.runGet (canMessageData msg) of
    Left e -> error e
    Right SDOReplyUploadInit{..} -> do
      Control.Monad.unless
        (sdoReplyUploadInitMux == mux)
        $ throw
            $ SDOExceptionDetail_MuxMismatch
                mux
                sdoReplyUploadInitMux

      let SDOInit{..} = sdoReplyUploadInitHeader
      case sdoInitExpedited of
        True ->
          case sdoInitSizeIndicated of
            True -> do
              let dataLength = 4 - (fromIntegral sdoInitNumBytes)
              pure
                $ take dataLength
                $ drop 4 (canMessageData msg)

            False -> throw SDOExceptionDetail_ExpeditedNoSizeIndicated
        False -> throw SDOExceptionDetail_UnsupportedSegmentedTransfer
    Right other -> throw $ SDOExceptionDetail_UnexpectedSDOReply "SDOReplyUploadInit" other
  where
    throw = throwIO . SDOException_Read

-- aka write (download) to device
sdoClientDownload
  :: MonadThrow m
  => CAN m
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

  canSend
    can
    $ CANMessage
        (sdoRequestID nID)
        $ (Network.CANOpen.Serialize.runPut
            $ SDORequestDownloadInit header mux
          )
          ++ bytes

  msg <- canRecv can

  Control.Monad.unless
    (canMessageArbitrationField msg == sdoReplyID nID)
    $ throw
        $ SDOExceptionDetail_SDOReplyBadID
            (sdoReplyID nID)
            (canMessageArbitrationField msg)

  case Network.CANOpen.Serialize.runGet (canMessageData msg) of
    Left e -> error e
    Right SDOReplyDownloadInit{..} -> do
      Control.Monad.unless
        (sdoReplyDownloadInitMux == mux)
        $ throw
            $ SDOExceptionDetail_MuxMismatch
                mux
                sdoReplyDownloadInitMux

      pure ()
    Right other -> throw $ SDOExceptionDetail_UnexpectedSDOReply "SDOReplyDownloadInit" other
  where
    throw = throwIO . SDOException_Read

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
