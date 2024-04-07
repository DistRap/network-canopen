{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
module Util
  ( TestBusState(..)
  , TestBus(..)
  , runTestBus
  , testBus
  , busResult
  , roundtrips
  ) where

import Control.Monad.State (MonadState, State, gets, modify, runState)

import Network.CAN (MonadCAN(..), CANMessage)
import Test.Hspec (Expectation, shouldBe)

-- * TestBus

data TestBusState = TestBusState
  { testBusStateToSend :: [CANMessage]
  , testBusStateReceived :: [CANMessage]
  } deriving (Eq, Ord, Show)

newtype TestBus a = TestBus { unTestBus :: State TestBusState a }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadState TestBusState
           )

instance MonadCAN TestBus where
  send msg = do
    modify
      (\x -> x { testBusStateReceived = testBusStateReceived x ++ [msg] })

  recv = do
    msgs <- gets testBusStateToSend
    case msgs of
      (x:xs) -> do
        modify
          (\s -> s { testBusStateToSend = xs })
        pure x
      _ -> error "TestBus: No more messages to send"

runTestBus
  :: TestBusState
  -> TestBus a
  -> (a, TestBusState)
runTestBus s =
  (`runState` s)
  . unTestBus

testBus
  :: [CANMessage]
  -> TestBus a
  -> (a, TestBusState)
testBus toSend =
  runTestBus
    TestBusState
      { testBusStateToSend = toSend
      , testBusStateReceived = mempty
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

