module Network.CANOpen.NMT.Types
  ( NMTState(..)
  ) where

data NMTState
  = NMTState_Initialising
  | NMTState_Resetting
  | NMTState_ResettingComm
  | NMTState_PreOperational
  | NMTState_Operational
  | NMTState_Stopped
  deriving (Bounded, Eq, Enum, Ord, Show)
