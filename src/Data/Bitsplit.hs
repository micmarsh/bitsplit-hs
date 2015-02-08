module Data.Bitsplit where
import Data.Bitsplit.Types
import Data.Bitsplit.Calc

hello name = "Hello, " ++ name ++ "!"

main = (putStrLn . hello) "Bitsplit"
