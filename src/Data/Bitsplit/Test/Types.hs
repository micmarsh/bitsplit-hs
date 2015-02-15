{-# LANGUAGE MultiWayIf #-}
module Data.Bitsplit.Test.Types where
import Data.Bitsplit.Types
import Data.Natural
import Data.Ratio
import Control.Applicative ((<$>))
import Test.QuickCheck

addresses = ["1LizTeRZKA2MhdjjzCcFpvqfSRYSunrCsW",
            "1HxoFaq36tAvAsXMzwcmRDoBK6CTZiek4C",
            "1F34duy2eeMz5mSrvFepVzy7Y1rBsnAyWC",
            "16g2TW2hbFoov3DThSRBdnETaS94bFvq5v",
            "1FpqQnKQCgDkJFMC94JL8FpRyHTZ3uRVZ1",
            "1f1miYFQWTzdLiCBxtHHnNiW7WAWPUccr",
            "1vFzbs8hN4Y8EtgcKpfTUuSQVoq2MApQ6",
            "1BfCR6RdQ7x3FLgZZfrW6dgpLuqnKqarGo",
            "1LiWe7QnM4vgQZJV5cL5yPiebWXFgynu9a",
            "13D5DVAyC2tH9P7ab9UonNCUXjbAxKVWMb",
            "1PxJE5jxUp8BsTb819k6Qbqk5crWF6HMMK",
            "1PYwPRCdYHtbHbpkDdRFToo1GKCgAXbGvL",
            "1PqSXncpDgVZhYmKbLTC9ZZAqJYkE1SfJD",
            "15vbwgCa9dFqbCfSCg1iMKFFqMpyKa482a",
            "1BrzsWUf6L2BLaGs7YGsM6n2zdeRhWtjyU",
            "1Ct9r9piNCbzvWtM4UFZhKFFriNYLt4r64",
            "1HGZdSdtWSgPMFsBy7QuFamV2TPSawfbxA",
            "1C4JhThVN6ZSHwbMmtTJ1oPFdv4JSLTc6d",
            "15f5xWsQ8iXYEMu7fHZP95hzNYyHFF8yo1",
            "12yyaqLdWdmMjFee47zQ68zqzX8ry9BfDm"]

unsafeUnpack :: Maybe a -> a
unsafeUnpack (Just value) = value

instance Arbitrary Natural where
         arbitrary =  fromInteger <$> (choose (0, 999999))

instance Arbitrary Address where
         arbitrary = elements $ (unsafeUnpack . mkAddress) <$> addresses

newtype ArbPosRatio = ArbPosRatio (Ratio Natural)

instance Arbitrary ArbPosRatio where
         arbitrary =  do
                   nat1 <- arbitrary
                   nat2 <- arbitrary
                   let least = min nat1 nat2
                       most = max nat1 nat2
                   return $ ArbPosRatio (least % most)

newtype ArbOneVec = ArbOneVec [Ratio Natural]

accumulateRatios times ratios = do
                 (ArbPosRatio new) <- arbitrary
                 let newList = new : ratios
                     total = sum newList
                 if | total == 1 -> return $ ArbOneVec newList
                    | times >= 100 -> (return . ArbOneVec) $ (1 - (sum ratios)) : ratios
                    | total < 1 -> accumulateRatios (times + 1) newList
                    | otherwise -> accumulateRatios (times + 1) ratios

instance Arbitrary ArbOneVec where
         arbitrary = accumulateRatios 0 []

newtype ArbPreSplit = ArbPreSplit [(Address, Ratio Natural)]

instance Arbitrary ArbPreSplit where
         arbitrary = do
                   addr <- arbitrary
                   (ArbPosRatio ratio) <- arbitrary
                   (ArbOneVec one) <- arbitrary
                   let tuple = return (addr, ratio)
                       randomList = (listOf tuple)
                       addressList = (unsafeUnpack . mkAddress) <$> addresses
                       goodList = return $ zip addressList one
                   ArbPreSplit <$> oneof [randomList, goodList]

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True

instance Arbitrary Split where
         arbitrary = let maybeSplit = make <$> arbitrary
                         resolveSplit = suchThat maybeSplit isRight
                     in unpack <$> resolveSplit
                     where make (ArbPreSplit ps) = mkSplit ps
                           unpack (Right unsafe) = unsafe
