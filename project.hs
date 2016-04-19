import Data.Time.Clock
import Data.Time.Calendar

taxRate = 0.065

digitize :: Int -> [Int]
digitize x = digitize (x `div` 10) ++ [x `mod` 10]

cardType :: Int -> String
cardType x | (head (show x)) == '4' = "Visa"
	   | (head (show x)) == '5' = "MasterCard" 
	   | (head (show x)) == '6' = "Discover"
	   | otherwise = "Not Accepted"

pastDate :: Integer -> Int -> Int -> IO Bool
pastDate x y z = do
   d1 <- fmap utctDay getCurrentTime
   return ((fromGregorian x y z) > d1)

calcTax :: Float -> Float
calcTax x = x * taxRate 

checkOut :: Float -> IO ()
checkOut num = do
   putStrLn "Enter cost: "
   cost <- getLine
   let amount = (read cost :: Float)
   if amount /= 0
      then do putStrLn (show amount)
              checkOut (amount + num)
   else putStrLn (show (amount + (num + (calcTax (amount + num)))))

verifyCard :: Int -> Integer -> Int -> Int  -> IO Bool
verifyCard  cardNo year month day = do
   d2 <- pastDate year month day
   if (cardType cardNo == "Not Accepted")
	then return False
	else if (d2)
	   then return False
	   else return True 
