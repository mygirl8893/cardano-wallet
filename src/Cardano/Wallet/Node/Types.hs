module Cardano.Wallet.Node.Types
    ( EpochIndex, SlotNumber, Slot ) where

import Data.Word
    ( Word16, Word64 )
import Prelude

-- fixme: add to Primitive types
type EpochIndex = Word64
type SlotNumber = Word16
type Slot = (EpochIndex, SlotNumber)
