
module Network.CANOpen.PDO
  ( PDOMapEntry(..)
  , PDO(..)
  , pdoMapArray
  , rpdo1
  , tpdo1
  , pdoBaseIndex
  , pdoMapSize
  , writePDOMap
  , readPDOMapEntries
  , mkDictionary
  , readPDOMap
  ) where

import Data.Proxy (Proxy(Proxy))
import Data.Word (Word8)
import Data.Map (Map)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat, natVal)
import Network.CANOpen.Class (MonadNode(..))
import Network.CANOpen.Types
  ( Array(..)
  , Index(..)
  , Mux(..)
  , Permission(..)
  , SomeFixedSized(..)
  , SubIndex(..)
  , Variable(..)
  , mkArray
  , variableSize
  )
import Network.CANOpen.Serialize (CSerialize(..))
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))

import qualified Data.Map.Strict

data PDOMapEntry = PDOMapEntry
  { pdoMapEntryMux     :: Mux
  , pdoMapEntryLength  :: Word8
  } deriving (Eq, Generic, Ord, Show)
    deriving (Arbitrary) via GenericArbitrary PDOMapEntry

-- This is a Word32
-- [ Index (W16), SubIndex (W8), Length (W8) ]
-- which on the wire ends up as
-- [ Length (W8), SubIndex (W8), Index (W16), ]
-- so we have to put/get it in reverse order
instance CSerialize PDOMapEntry where
  put PDOMapEntry{..} = do
    put pdoMapEntryLength
    put $ unSubIndex $ muxSubIndex pdoMapEntryMux
    put $ unIndex $ muxIndex pdoMapEntryMux

  get = do
    pdoMapEntryLength <- get
    subIdx <- SubIndex <$> get
    idx <- Index <$> get
    let
      pdoMapEntryMux = Mux idx subIdx
    pure PDOMapEntry{..}

data PDO (n :: Nat)
  = PDO_Receive
  | PDO_Transmit

pdoMapArray
  :: forall n . KnownNat n
  => PDO n
  -> Array PDOMapEntry
pdoMapArray pdo =
  mkArray
    ( (case pdo of
        PDO_Receive  -> "RPDO"
        PDO_Transmit -> "TPDO"
      )
      <> show idx
      <> "Map"
    )
    ( (case pdo of
        PDO_Receive  -> 0x1600
        PDO_Transmit -> 0x1A00
      )
      + fromIntegral idx
    )
    Permission_ReadWrite
  where
    idx = natVal (Proxy @n)

rpdo1 :: PDO 1
rpdo1 = PDO_Receive

tpdo1 :: PDO 1
tpdo1 = PDO_Transmit

-- | Starting index (memory address) of the PDO
--
-- Used to assign PDO to SyncManager (EtherCAT)
pdoBaseIndex
  :: forall n . KnownNat n
  => PDO n
  -> Index
pdoBaseIndex =
    muxIndex
  . variableMux
  . arrayCount
  . pdoMapArray

-- | Full length of the mapping in bytes
pdoMapSize
  :: [SomeFixedSized Variable]
  -> Int
pdoMapSize =
    fromIntegral
  . sum
  . map (\(SomeFixedSized v) -> variableSize v)

writePDOMap
  :: ( KnownNat n
     , MonadNode m
     )
  => PDO n
  -> [SomeFixedSized Variable]
  -> m ()
writePDOMap pdo mappings = do
  sdoWriteArray
    (pdoMapArray pdo)
    $ map asEntry mappings
  where
    asEntry :: SomeFixedSized Variable -> PDOMapEntry
    asEntry (SomeFixedSized var) =
      PDOMapEntry
        { pdoMapEntryMux    = variableMux var
        , pdoMapEntryLength = variableSize var
        }

readPDOMapEntries
  :: ( KnownNat n
     , MonadNode m
     )
  => PDO n
  -> m [PDOMapEntry]
readPDOMapEntries = sdoReadArray . pdoMapArray

mkDictionary
  :: [SomeFixedSized Variable]
  -> Map Mux (SomeFixedSized Variable)
mkDictionary =
    Data.Map.Strict.fromList
  . map (\s@(SomeFixedSized v) -> (variableMux v, s))

-- Read typed PDO mapping, requires supplying a dictionary
-- which can be constructed using @mkDictionary@ from
-- a predefined PDO mapping, for example:
--
-- > tPDOMap :: [SomeFixedSized Variable]
-- > tPDOMap =
-- >   [ SomeFixedSized controlWord
-- >   , SomeFixedSized targetPosition
-- >   ]
-- >
-- > rPDOMap :: [SomeFixedSized Variable]
-- > rPDOMap =
-- >   [ SomeFixedSized statusWord
-- >   , SomeFixedSized positionActual
-- >   ]
-- >
-- > dictionary
-- >   :: Map Mux (SomeFixedSized Variable)
-- > dictionary =
-- >   mkDictionary $ rPDOMap ++ tPDOMap
readPDOMap
  :: ( KnownNat n
     , MonadNode m
     )
  => PDO n
  -> Map Mux (SomeFixedSized Variable)
  -> m [Maybe (SomeFixedSized Variable)]
readPDOMap pdo dict = do
  fmap
    (flip
      Data.Map.Strict.lookup
      dict
    . pdoMapEntryMux)
  <$> readPDOMapEntries pdo
