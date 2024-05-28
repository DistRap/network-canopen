{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Monad.Reader
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.Map
import qualified UnliftIO.Async

nID :: NodeID
nID = NodeID 1

newtype TBus m a = TBus { unTBus :: ReaderT (TMVar CANMessage) m a }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadIO
           , MonadReader (TMVar CANMessage)
           , MonadTrans
           )

instance (MonadIO m, MonadCAN m) => MonadCAN (TBus m) where
  send = lift . send
  recv = ask >>= liftIO . atomically . takeTMVar

runTBus
  :: Monad m
  => TMVar CANMessage
  -> TBus m a
  -> m a
runTBus s =
  (`runReaderT` s)
  . unTBus

main :: IO ()
main = do
  let
    l :: (MonadIO m, Show a) => a -> m ()
    l = liftIO . print

    doLSS = not True -- False

  cs <- newCANOpenState

  tb <- newEmptyTMVarIO
  tcmd :: TMVar (TBus (CANOpenT (Network.SocketCAN.SocketCANT IO)) a) <- newEmptyTMVarIO

  UnliftIO.Async.async $ do
   forever $ do
     atomically
       $ writeTMVar
           tcmd
           $ sdoClientUpload @Int32 nID (Mux 0x606C 0) -- undefined :: _

     atomically
       $ writeTMVar
           tcmd
           $ sdoClientUpload @Word32 nID (Mux 0x606C 0) -- undefined :: _

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

      UnliftIO.Async.async $ do
        runTBus tb $ do
          -- read vendor id
          sdoClientUpload @Word32 nID (Mux 0x1018 1)

          -- at 0x606C $ field "velocity_actual" sint32 & ro
          sdoClientUpload @Int32 nID (Mux 0x606C 0) >>= l
          -- at 0x60FF $ field "target_velocity" sint32
          sdoClientDownload @Int32 nID (Mux 0x60FF 0) (123242)

          forever $ do
            cmd <- liftIO . atomically . takeTMVar $ tcmd
            cmd >>= l
            --sdoClientUpload @Int32 nID (Mux 0x606C 0) >>= l

      registerHandler
        (sdoReplyID nID)
        (atomically . writeTMVar tb)

      forever $ do
        msg@CANMessage{..} <- recv
        handlers <- asks canOpenStateHandlers >>= liftIO . readTVarIO
        forM_
          (Data.Map.toList handlers)
          (\(arb, handler) ->
            when
              (arb == canMessageArbitrationField)
              $ liftIO
              $ handler msg
          )

      pure ()
  >>= print
