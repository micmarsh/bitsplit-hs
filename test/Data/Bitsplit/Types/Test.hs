module Data.Bitsplit.Types.Test where

import Data.Set (fromList, toList)
import Data.Ratio
import Data.Natural
import Data.Bitsplit.Types
import Data.Bitsplit.Test.Types
import Test.QuickCheck
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

tests :: [Test]
tests =
      [testGroup "Validated Split Type"
                 [ testProperty "Addresses unique" (splitCheck testUniqueAddresses)
                 , testProperty "Amounts sum to zero for empty, one otherwise"
                                (splitCheck testOneOrZero)]]

splitCheck :: ([(Address, Ratio Natural)] -> Bool) -> Split -> Bool
splitCheck pred = pred . unpackSplit

testUniqueAddresses :: [(Address, Ratio Natural)] -> Bool
testUniqueAddresses splits =
                    let addresses = fmap fst splits
                        uniqueAddrs = (toList . fromList) addresses
                    in length uniqueAddrs == length addresses

testOneOrZero :: [(Address, Ratio Natural)] -> Bool
testOneOrZero splits =
              let amounts = fmap snd splits
                  total = foldl (+) 0 amounts
              in null amounts || total == 1

splitProp :: ([(Address, Ratio Natural)] -> Bool) -> Property
splitProp = (forAll arbitrary) . splitCheck

uniqueAddressProp = splitProp testUniqueAddresses
oneOrZeroProp = splitProp testOneOrZero
