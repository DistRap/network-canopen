module Util
  ( TestBusState(..)
  , withTestBus
  , busResult
  , roundtrips
  ) where

import Control.Monad.State (State, gets, modify, runState)

import Network.CAN (CANMessage, CANEndpoint(..))
import Test.Hspec (Expectation, shouldBe)

-- * TestBus

data TestBusState = TestBusState
  { testBusStateToSend   :: [CANMessage]
  , testBusStateReceived :: [CANMessage]
  } deriving (Eq, Ord, Show)

withTestBus
  :: [CANMessage]
  -> (CANEndpoint (State TestBusState) -> State TestBusState a)
  -> (a, TestBusState)
withTestBus toSend act =
  runState (act testBusEndpoint) initialState
  where
    initialState =
      TestBusState
        { testBusStateToSend = toSend
        , testBusStateReceived = mempty
        }

    testBusEndpoint :: CANEndpoint (State TestBusState)
    testBusEndpoint = CANEndpoint
      { canEndpointSend = \msg ->
          modify (\x ->
            x { testBusStateReceived = testBusStateReceived x ++ [msg] })

      , canEndpointRecv = do
          msgs <- gets testBusStateToSend
          case msgs of
            (x:xs) -> do
              modify (\s -> s { testBusStateToSend = xs })
              pure x
            [] ->
              error "TestBus: No more messages to send"
      }

busResult
  :: [CANMessage]
  -> TestBusState
busResult xs =
  TestBusState
    { testBusStateToSend = mempty
    , testBusStateReceived = xs
    }

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

