module Util
  ( TestBusResult(..)
  , withTestBus
  , okBusResult
  , shouldReturnInSim
  , roundtrips
  ) where

import Control.Concurrent.Class.MonadSTM.TQueue (newTQueue, tryReadTQueue, flushTQueue, writeTQueue, labelTQueue)
import Control.Monad
import Control.Monad.Class.MonadThrow (Exception, MonadThrow(throwIO))
import Control.Monad.Class.MonadSTM (MonadSTM(atomically), MonadLabelledSTM)
import Control.Monad.IOSim

import Network.CAN (CANMessage, CAN(..))
import Test.Hspec (Expectation , HasCallStack, expectationFailure, shouldBe)

-- * TestBus

data TestBusResult = TestBusResult
  { testBusResultNotSent  :: [CANMessage]
  , testBusResultReceived :: [CANMessage]
  } deriving (Eq, Ord, Show)

data TestBusException
  = TestBusException_NothingMoreToSend
  deriving Show

instance Exception TestBusException

withTestBus
  :: ( MonadThrow m
     , MonadLabelledSTM m
     )
  => [CANMessage]
  -> (CAN m -> m a)
  -> m (a, TestBusResult)
withTestBus toSend act = do
  (sendQueue, recvQueue) <- atomically $ do
    sQ <- newTQueue
    labelTQueue sQ "sendQueue"
    forM_ toSend $ writeTQueue sQ

    rQ <- newTQueue
    labelTQueue rQ "recvQueue"
    pure (sQ, rQ)

  x <-
    act
      $ CAN
        { canSend = atomically . writeTQueue recvQueue
        , canRecv =
            atomically
              (tryReadTQueue sendQueue)
            >>= \case
              Nothing -> throwIO TestBusException_NothingMoreToSend
              Just x -> pure x
        }

  busRes <-
    atomically
      $ TestBusResult
          <$> flushTQueue sendQueue
          <*> flushTQueue recvQueue

  pure (x, busRes)

okBusResult
  :: [CANMessage]
  -> TestBusResult
okBusResult xs =
  TestBusResult
    { testBusResultNotSent  = mempty
    , testBusResultReceived = xs
    }

-- | Run action in IOSim, compare result
-- if it passes, render simulation trace
-- if not.
shouldReturnInSim
  :: (HasCallStack, Show a, Eq a)
  => (forall s. IOSim s a)
  -> a
  -> Expectation
shouldReturnInSim action expected =
  let
    tr = runSimTrace action
  in
    case traceResult True tr of
      Right r -> r `shouldBe` expected
      Left e ->
        expectationFailure
          $    show e
            <> "\n"
            <> ppTrace tr

--

roundtrips
  :: forall a b f
   . ( Applicative f
     , Eq (f a)
     , Show a
     , Show b
     , Show (f a)
     )
  => (a -> b)   -- ^ Encode
  -> (b -> f a) -- ^ Decode
  -> a
  -> Expectation
roundtrips encode decode x =
  decode (encode x) `shouldBe` pure x

