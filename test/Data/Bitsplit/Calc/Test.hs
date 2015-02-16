module Data.Bitsplit.Calc.Test where

import Data.Bitsplit.Calc (deleteSplit, upsertSplit, deduct)
import Data.Bitsplit.Types (unpackSplit, Address, Split)
import Data.Natural
import Data.Ratio
import Data.Bitsplit.Test.Calc (arbArgs)
import Test.QuickCheck (Property, forAll, arbitrary)

type UnpackedSplit = [(Address, Ratio Natural)]

testProp :: (Address -> Ratio Natural -> Split -> Maybe Split) ->
            (UnpackedSplit -> UnpackedSplit -> Bool) -> Property
testProp call compare = forAll arbArgs checkResult
         where checkResult (address, ratio, split) =
                       case call address ratio split of
                            Nothing -> False
                            (Just newSplit) ->
                                  let oldList = unpackSplit split
                                      newList = unpackSplit newSplit
                                  in compare oldList newList

checkLengths diff old new = null old || length old == length new || diff (length old) == length new

deleteSplit' address _ split = deleteSplit address split
checkDelete = checkLengths (subtract 1)
deletesAddr :: Property
deletesAddr = testProp deleteSplit' checkDelete

checkUpsert = checkLengths (+ 1)
upsertsAddr :: Property
upsertsAddr = testProp upsertSplit checkUpsert
