module SerializeSpec (spec) where

import Data.Serialize.Get (runGet)
import Data.Serialize.Put (runPut)
import Test.Hspec (Expectation, Spec, describe, parallel)
import Test.Hspec.QuickCheck (prop)
import Util (roundtrips)

import Network.CANOpen.Serialize (CSerialize(..))
import Network.CANOpen.Types (Mux)
import Network.CANOpen.SDO.Types (SDORequest, SDOReply)

-- | Test for roundtrip using @CSerialize@ instance
roundtripS
  :: ( Eq a
     , CSerialize a
     , Show a
     )
  => a
  -> Expectation
roundtripS =
  roundtrips
    (runPut . put)
    (runGet get)

spec :: Spec
spec = parallel $ do
  describe "Types" $ do
    prop "Mux" $ roundtripS @Mux
  describe "SDO" $ do
    prop "SDORequest" $ roundtripS @SDORequest
    prop "SDOReply" $ roundtripS @SDOReply
