{-# LANGUAGE TemplateHaskell #-}
module Network.CANOpen.LSS where

import Control.Lens (Prism')
import Data.Word (Word32)
import Network.CAN (CANArbitrationField, CANMessage(..), CAN(..))
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
  :: Monad m
  => CAN m
  -> [LSSRequest]
  -> Prism' LSSReply a
  -> m a
lssReq can reqs respLens = do
  let
    sendReq = canSend can . lssRequest

  Control.Monad.mapM_ sendReq reqs

  -- TODO: timeout
  -- for example when there's no unconfed device
  -- identifyNonConfigured would just hang now
  lssReply <$> canRecv can
  >>= \case
    Left e -> error e
    Right lssResp ->
      case Control.Lens.preview respLens lssResp of
        Just r -> pure r
        Nothing -> error $ "Wrong lss reply" <> show lssResp

switchModeGlobal
  :: Monad m
  => CAN m
  -> LSSMode
  -> m ()
switchModeGlobal can =
  canSend can . lssRequest . LSSRequest_SwitchGlobal

configNodeID
  :: Monad m
  => CAN m
  -> NodeID
  -> m LSSConfigNodeIDStatus
configNodeID can nID =
  lssReq
    can
    (pure $ LSSRequest_ConfigNodeID nID)
    _LSSReply_ConfigNodeID

storeConfig
  :: Monad m
  => CAN m
  -> m LSSStoreConfigStatus
storeConfig can =
  lssReq
    can
    (pure LSSRequest_StoreConfig)
    _LSSReply_StoreConfig

inquireVendor
  :: Monad m
  => CAN m
  -> m Word32
inquireVendor can =
  lssReq
    can
    (pure LSSRequest_InquireVendor)
    _LSSReply_InquireVendor

inquireNodeID
  :: Monad m
  => CAN m
  -> m NodeID
inquireNodeID can =
  lssReq
    can
    (pure LSSRequest_InquireNodeID)
    _LSSReply_InquireNodeID

identifyNonConfigured
  :: Monad m
  => CAN m
  -> m ()
identifyNonConfigured can =
  lssReq
    can
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
