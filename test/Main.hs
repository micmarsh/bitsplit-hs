module Main where

import Test.Framework (defaultMain)
-- verbose debugging
import Test.QuickCheck (verboseCheck, (.&&.) )

-- Type Tests
import qualified Data.Bitsplit.Types.Test as Types

main :: IO ()
main = verboseCheck (Types.oneOrZeroProp .&&.
                    Types.uniqueAddressProp)
