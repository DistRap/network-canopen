# network-canopen

[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/DistRap/network-canopen/ci.yaml?branch=main)](https://github.com/DistRap/network-canopen/actions/workflows/ci.yaml)
[![Hackage version](https://img.shields.io/hackage/v/network-canopen.svg?color=success)](https://hackage.haskell.org/package/network-canopen)
[![Dependencies](https://img.shields.io/hackage-deps/v/network-canopen?label=Dependencies)](https://packdeps.haskellers.com/feed?needle=network-canopen)

CANOpen

## Usage

```haskell
import qualified Network.CANOpen

main :: IO ()
main = do
  runCANOpen <HOW> $ do
    io <- addNode 1
    forAllNodes waitBootup
    forAllNodes setOperational

    let outputAddr = VariableAddress (Index 0x6000) (SubIndex 1)

    sdoWrite io outputAddr 0b1
    sdoRead io outputAddr
      >>= liftIO . print
```
