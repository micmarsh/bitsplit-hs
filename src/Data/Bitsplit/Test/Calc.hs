module Data.Bitsplit.Test.Calc where
import Data.Bitsplit.Test.Types
import Data.Bitsplit.Types
import Data.Natural
import Data.Ratio
import Test.QuickCheck

arbSplitArgs :: Gen (Address, Ratio Natural, Split)
arbSplitArgs = do
        (ArbPosRatio ratio) <- arbitrary
        split <- arbitrary
        let existingAddrs = ((fmap fst) . unpackSplit) split
        address <- if null existingAddrs then arbitrary
                   else oneof [arbitrary, elements existingAddrs]
        return (address, ratio, split)

arbDeductArgs :: Gen (Ratio Natural, [Ratio Natural])
arbDeductArgs = do
              (ArbPosRatio ratio) <- arbitrary
              (ArbOneVec vector) <- arbitrary
              return (ratio, vector)
