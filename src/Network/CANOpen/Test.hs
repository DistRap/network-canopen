
module Network.CANOpen.Test where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Network.CAN
import Network.CANOpen
import Network.CANOpen.Types
import Network.CANOpen.LSS
import Network.SocketCAN

nID :: NodeID
nID = NodeID 1

main :: IO ()
main = do
  let
    l :: (MonadIO m, Show a) => a -> m ()
    l = liftIO . print

  cs <- newCANOpenState

  runSocketCAN "vcan0" $ do
    runCANOpenT cs $ do
      configNodeID nID >>= l
      inquireNodeID >>= l
      storeConfig >>= l
      recv >>= l
  >>= print
