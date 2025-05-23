{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.CANOpen.SubBus
  ( SubBus
  , runSubBus
  ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, takeTMVar)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT(..))
import Network.CAN (CANMessage, MonadCAN(..))
import UnliftIO (MonadUnliftIO)

-- | SubBus is used to run a nested
-- client (like SDO client), sends are
-- propagated to parent bus while
-- reads are routed by the application
-- via a handler registered for specific
-- message IDs via a TMVar.
--
-- This allows serialized client-server like
-- communication.
newtype SubBus m a =
  SubBus { _unSubBus :: ReaderT (TMVar CANMessage) m a }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadIO
           , MonadUnliftIO
           , MonadReader (TMVar CANMessage)
           , MonadTrans
           )

instance (MonadIO m, MonadCAN m) => MonadCAN (SubBus m) where
  send = lift . send
  recv = ask >>= liftIO . atomically . takeTMVar

runSubBus
  :: Monad m
  => TMVar CANMessage
  -> SubBus m a
  -> m a
runSubBus s =
  (`runReaderT` s)
  . _unSubBus
