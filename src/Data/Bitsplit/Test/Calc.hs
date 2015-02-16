module Data.Bitsplit.Test.Calc where
import Data.Bitsplit.Test.Types
import Data.Bitsplit.Types
import Data.Natural
import Data.Ratio
import Control.Applicative ((<$>))
import Test.QuickCheck



arbArgs :: Gen (Address, Ratio Natural, Split)
arbArgs = do
        (ArbPosRatio ratio) <- arbitrary
        split <- arbitrary
        let existingAddrs = ((fmap fst) . unpackSplit) split
        address <- if null existingAddrs then arbitrary
                   else oneof [arbitrary, elements existingAddrs]
        return (address, ratio, split)
