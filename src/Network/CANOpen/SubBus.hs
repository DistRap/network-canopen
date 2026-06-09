module Network.CANOpen.SubBus
  ( withSubBus
  ) where

import Control.Monad.Class.MonadSTM
import Control.Concurrent.Class.MonadSTM.TMVar

import Network.CAN (CANMessage, CAN(..))

-- | SubBus is used to run a nested
-- client (like SDO client), sends are
-- propagated to parent bus while
-- reads are routed by the application
-- via a handler registered for specific
-- message IDs via a TMVar.
--
-- This allows serialized client-server like
-- communication.

withSubBus
  :: MonadSTM m
  => CAN m
  -> TMVar m CANMessage
  -> (CAN m -> m a)
  -> m a
withSubBus parentBus tmVar act =
  act
    $ CAN
        { canSend = canSend parentBus
        , canRecv = atomically $ takeTMVar tmVar
        }
