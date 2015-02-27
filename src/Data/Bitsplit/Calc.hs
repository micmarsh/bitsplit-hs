{-# LANGUAGE MultiWayIf #-}
module Data.Bitsplit.Calc
(deleteSplit, upsertSplit, deduct) where
import Data.Bitsplit.Types
import Data.Ratio
import Data.Natural
import Data.List (partition, genericLength)
import Data.Set (fromList, member)

import Debug.Trace
traceMessage :: (Show a) => String -> a -> a
traceMessage message item =
             trace (message ++ ":\n" ++ show item) item

mkSplit' :: [(Address, Ratio Natural)] -> Maybe Split
mkSplit' split =
         case mkSplit split of
         (Left _) -> Nothing
         (Right value) -> Just value

zero :: Num a => b -> a
zero _ = 0

subtractPos :: (Num a, Ord a) => a -> a -> ([a],a) -> ([a],a)
subtractPos toSub num (soFar, remaining) =
            if | num == 0 -> (soFar, remaining)
               | toSub > num -> (0:soFar, remaining + toSub - num)
               | otherwise -> ((num - toSub):soFar, remaining)

-- no fear of number not between 0 and 1!
deduct :: (Show a, Integral a) => Ratio a -> [Ratio a] -> [Ratio a]
deduct amount [] = []
deduct 0 numbers = numbers
deduct 1 numbers = fmap zero numbers
deduct amount [number] = [number - amount]
deduct amount numbers =
       let total = genericLength $ filter (> 0) numbers
           toSub = amount / total
           (result, remaining) = foldr (subtractPos toSub) ([],0) numbers
       in if remaining == 0 then result
       else deduct remaining result

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
                newSplits = zip (addr : addresses) (amount : deduct amount amounts)
            in mkSplit' newSplits
