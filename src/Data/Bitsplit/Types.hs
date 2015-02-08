{-# LANGUAGE MultiWayIf #-}
module Data.Bitsplit.Types
(Address, mkAddress, Split, mkSplit, unpackSplit, isEmpty) where
import Data.Ratio
import Data.Natural
import Data.Map (fromList, toList)
--import qualified Network.Bitcoin.Types as BT

newtype Address = Address String deriving (Eq, Show, Ord) --BT.Address

mkAddress :: String -> Maybe Address
mkAddress = Just . Address --BT.mkAddress

newtype Split = Split [(Address, Ratio Natural)] deriving (Show, Eq)
-- Guarantees from constructor (gen test this!):
   -- Every Address is unique
   -- All of the "values" add up to exactly one, or zero if empty
   -- Because of the above, every number is [0, 1]


split = Right . Split

dedupe :: Ord a => [(a, b)] -> [(a, b)]
dedupe = toList . fromList

mkSplit :: [(Address, Ratio Natural)] -> Either String Split
mkSplit [] = split []
mkSplit [(address, _)] = split [(address, 1)]
mkSplit ratios =
        let deduped = dedupe ratios
            total = foldl (+) 0 $ fmap snd deduped
        in if -- | True -> split deduped -- use for testing failing stuff
              | length deduped < length ratios -> Left "Duplicate addresses in list"
              | total == 1 -> split deduped
              | otherwise ->  Left "Total not equal to 1"

isEmpty :: Split -> Bool
isEmpty (Split []) = True
isEmpty _ = False

unpackSplit :: Split -> [(Address, Ratio Natural)]
unpackSplit (Split split) = split
