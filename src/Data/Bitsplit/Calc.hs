{-# LANGUAGE MultiWayIf #-}
module Data.Bitsplit.Calc (deleteSplit, upsertSplit, deduct) where
import Data.Bitsplit.Types
import Data.Ratio
import Data.Natural
import Data.List (partition, genericLength)

rmFirst :: (a -> Bool) -> [a] -> [a]
rmFirst pred list =
        let (remove, rest) = partition pred list
        in if length remove >= 1 then (tail remove) ++ rest
        else list

mkSplit' :: [(Address, Ratio Natural)] -> Maybe Split
mkSplit' split =
         case mkSplit split of
         (Left _) -> Nothing
         (Right value) -> Just value

-- no fear of number not between 0 and 1!
deduct :: Integral a => Ratio a -> [Ratio a] -> [Ratio a]
deduct amount [] = [amount]
deduct 0 numbers = 0 : numbers
deduct 1 numbers = 1 : fmap (\x -> 0) numbers
deduct amount [number] = [amount, number - amount]
deduct amount numbers =
       let toSub = amount / (genericLength numbers)
           subtracted = fmap (subtract toSub) numbers
           least = foldl min 0 subtracted
       in if least < 0
       then let noLeast = rmFirst (== least) subtracted
                adjusted = deduct (- least) noLeast
            in amount : 0 : adjusted
       else amount : subtracted

deleteSplit :: Address -> Split -> Maybe Split
deleteSplit address split =
            mkSplit' $ unsafeDeleteSplit address (unpackSplit split)

unsafeDeleteSplit :: Address -> [(Address, Ratio Natural)] -> [(Address, Ratio Natural)]
unsafeDeleteSplit address split =
             let (match, rest) = partition ((== address) . fst) split
             in if length match == 0 then split
             else let [(_, amount)] = match
                      (addresses, amounts) = unzip rest
                      toAdd = amount / (genericLength amounts)
                  in zip addresses (fmap (+ toAdd) amounts)

upsertSplit :: Address -> Ratio Natural -> Split -> Maybe Split
upsertSplit addr amount split =
            let unpacked  = unpackSplit split
                withoutAddr = unsafeDeleteSplit addr unpacked
                amount = min amount 1
                (addresses, amounts) = unzip withoutAddr
                newSplits = zip (addr : addresses) (deduct amount amounts)
            in mkSplit' newSplits
