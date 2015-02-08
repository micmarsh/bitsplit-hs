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
            "1f1miYFQWTzdLiCBxtHHnNiW7WAWPUccr"]

unsafeUnpack :: Maybe a -> a
unsafeUnpack (Just value) = value

instance Arbitrary Natural where
         arbitrary =  fromInteger <$> (choose (0, 999999))

instance Arbitrary Address where
         arbitrary = elements $ (unsafeUnpack . mkAddress) <$> addresses

newtype ArbRatio = ArbRatio (Ratio Natural)

instance Arbitrary ArbRatio where
         arbitrary =  do
                   nat1 <- arbitrary
                   nat2 <- arbitrary
                   let least = min nat1 nat2
                       most = max nat1 nat2
                   return $ ArbRatio (least % most)

newtype ArbPreSplit = ArbPreSplit [(Address, Ratio Natural)]

instance Arbitrary ArbPreSplit where
         arbitrary = do
                   addr <- arbitrary
                   (ArbRatio ratio) <- arbitrary
                   let tuple = return (addr, ratio)
                   ArbPreSplit <$> (listOf tuple)

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True

instance Arbitrary Split where
         arbitrary = let maybeSplit = make <$> arbitrary
                         resolveSplit = suchThat maybeSplit isRight
                     in unpack <$> resolveSplit
                     where make (ArbPreSplit ps) = mkSplit ps
                           unpack (Right unsafe) = unsafe
