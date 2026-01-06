
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
  , PDOMapping(..)
  , writePDOMapping
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
  { pdoMapEntryMux       :: Mux   -- ^ Mapped variable mux
  , pdoMapEntryBitLength :: Word8 -- ^ Mapped variable size in bits
  } deriving (Eq, Generic, Ord, Show)
    deriving (Arbitrary) via GenericArbitrary PDOMapEntry

-- This is a Word32
-- [ Index (W16), SubIndex (W8), Length (W8) ]
-- which on the wire ends up as
-- [ Length (W8), SubIndex (W8), Index (W16), ]
-- so we have to put/get it in reverse order
instance CSerialize PDOMapEntry where
  put PDOMapEntry{..} = do
    put pdoMapEntryBitLength
    put $ unSubIndex $ muxSubIndex pdoMapEntryMux
    put $ unIndex $ muxIndex pdoMapEntryMux

  get = do
    pdoMapEntryBitLength <- get
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
      -- 1-indexed, we should also disallow @PDO 0@
      + (fromIntegral idx - 1)
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
        { pdoMapEntryMux       = variableMux var
        , pdoMapEntryBitLength = 8 * variableSize var
        }

readPDOMapEntries
  :: ( KnownNat n
     , MonadNode m
     )
  => PDO n
  -> m [PDOMapEntry]
readPDOMapEntries = sdoReadArray . pdoMapArray

data PDOMapping = PDOMapping
  { pdoMappingReceive  :: [SomeFixedSized Variable]
  , pdoMappingTransmit :: [SomeFixedSized Variable]
  } deriving (Show)

-- | Write @PDOMapping@ to nodes RPDO1/TPDO1 variables
writePDOMapping
  :: MonadNode m
  => PDOMapping
  -> m ()
writePDOMapping PDOMapping{..} = do
  writePDOMap
    rpdo1
    pdoMappingReceive

  writePDOMap
    tpdo1
    pdoMappingTransmit

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
-- > rPDOMap :: [SomeFixedSized Variable]
-- > rPDOMap =
-- >   [ SomeFixedSized controlWord
-- >   , SomeFixedSized targetPosition
-- >   ]
-- >
-- > tPDOMap :: [SomeFixedSized Variable]
-- > tPDOMap =
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
