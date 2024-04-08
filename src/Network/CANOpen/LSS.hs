{-# LANGUAGE TemplateHaskell #-}
module Network.CANOpen.LSS where

import Control.Lens (Prism')
import Data.Word (Word32)
import Network.CAN (MonadCAN(..), CANArbitrationField, CANMessage(..))
import Network.CANOpen.LSS.Types
  ( LSSMode
  , LSSRequest(..)
  , LSSReply(..)
  , LSSConfigNodeIDStatus
  , LSSStoreConfigStatus
  )
import Network.CANOpen.Types (NodeID(..))

import qualified Control.Lens
import qualified Control.Lens.TH
import qualified Control.Monad
import qualified Network.CAN
import qualified Network.CANOpen.Serialize

Control.Lens.TH.makePrisms ''LSSReply

lssReq
  :: MonadCAN m
  => [LSSRequest]
  -> Prism' LSSReply a
  -> m a
lssReq reqs respLens = do
  let
    sendReq = send . lssRequest

  Control.Monad.mapM_ sendReq reqs

  -- TODO: timeout
  -- for example when there's no unconfed device
  -- identifyNonConfigured would just hang now
  lssReply <$> recv
  >>= \case
    Left e -> error e
    Right lssResp ->
      case Control.Lens.preview respLens lssResp of
        Just r -> pure r
        Nothing -> error $ "Wrong lss reply" <> show lssResp

switchModeGlobal
  :: MonadCAN m
  => LSSMode
  -> m ()
switchModeGlobal =
  send . lssRequest . LSSRequest_SwitchGlobal

configNodeID
  :: MonadCAN m
  => NodeID
  -> m LSSConfigNodeIDStatus
configNodeID nID =
  lssReq
    (pure $ LSSRequest_ConfigNodeID nID)
    _LSSReply_ConfigNodeID

storeConfig
  :: MonadCAN m
  => m LSSStoreConfigStatus
storeConfig =
  lssReq
    (pure LSSRequest_StoreConfig)
    _LSSReply_StoreConfig

inquireVendor
  :: MonadCAN m
  => m Word32
inquireVendor =
  lssReq
    (pure LSSRequest_InquireVendor)
    _LSSReply_InquireVendor

inquireNodeID
  :: MonadCAN m
  => m NodeID
inquireNodeID =
  lssReq
    (pure LSSRequest_InquireNodeID)
    _LSSReply_InquireNodeID

identifyNonConfigured
  :: MonadCAN m
  => m ()
identifyNonConfigured =
  lssReq
    (pure LSSRequest_IdentifyNonConfigured)
    _LSSReply_IdentifyNonConfigured

lssRequest
  :: LSSRequest
  -> CANMessage
lssRequest req =
  CANMessage
    lssTXID
    $ Network.CANOpen.Serialize.pad 8
    $ Network.CANOpen.Serialize.runPut req

lssReply
  :: CANMessage
  -> Either String LSSReply
lssReply (CANMessage rxid dat) | rxid == lssRXID =
  Network.CANOpen.Serialize.runGet dat
lssReply (CANMessage rxid _dat) | otherwise =
  Left $ "Expected LSS RX ID, but got " <> show rxid

lssTXID :: CANArbitrationField
lssTXID = Network.CAN.standardID 0x7E5

lssRXID :: CANArbitrationField
lssRXID = Network.CAN.standardID 0x7E4
