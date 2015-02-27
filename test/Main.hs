module Main where

import Test.Framework (defaultMain)
-- verbose debugging
import Test.QuickCheck (verboseCheck, (.&&.) )

-- Actual Module Tests
import qualified Data.Bitsplit.Types.Test as Types
import qualified Data.Bitsplit.Calc.Test as Calc

main :: IO ()
main = verboseCheck (--Types.oneOrZeroProp .&&.
                     -- Types.uniqueAddressProp .&&.
                     --   Calc.popFirstWorks .&&.
                     --Calc.deletesAddr .&&.
                     -- Calc.deductsAmounts  .&&.
                     Calc.upsertsAddr
       )
