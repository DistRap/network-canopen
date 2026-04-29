# network-canopen

[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/DistRap/network-canopen/ci.yaml?branch=main)](https://github.com/DistRap/network-canopen/actions/workflows/ci.yaml)
[![Hackage version](https://img.shields.io/hackage/v/network-canopen.svg?color=success)](https://hackage.haskell.org/package/network-canopen)

Work-in-progress native CANOpen protocol master implementation.

## Usage

```haskell
import Data.Word (Word8)
import Network.CANOpen
  ( CANOpen(..)
  , CNode(..)
  , Mux(..)
  , NodeID(..)
  , Permission(..)
  , Variable(..)
  )

import qualified Network.CANOpen
import qualified Network.SocketCAN

outputs :: Variable Word8
outputs =
  Variable
  { variableName = "Outputs"
  , variableMux  = Mux 0x6000 1
  , variablePerm = Permission_ReadWrite
  }

main :: IO ()
main = do
  Network.SocketCAN.withSocketCAN (Network.SocketCAN.mkCANInterface "vcan0") $ \can ->
    Network.CANOpen.withCANOpen can $ \CANOpen{..} -> do

      io <- canOpenAddNode (NodeID 1)

      cNodeSDOWrite io outputs 0b1
      cNodeSDORead io outputs
      >>= putStrLn . show
```
