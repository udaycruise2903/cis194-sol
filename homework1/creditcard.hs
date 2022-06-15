{-# OPTIONS_GHC -Wall #-}


-- convert a 16 digit into a list of numbers
toDigits :: Integer -> [Integer]
toDigits (creditCardNum)
 | creditCardNum == 0 = []
 | otherwise = toDigits (creditCardNum `div` 10) ++ [creditCardNum `mod` 10]

{-
-- reverse the list of digits after converting from a number
toDigitsRev :: Integer -> [Integer]
toDigitsRev (num)
 | num == abs (num) = []
 | num == 0 = []
 | otherwise = reverse(toDigits(num))
-}

-- In a list, numbers at even position are doubled
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:(y:zs)) =  (x * 2) : y : doubleEveryOther zs


-- a sum is obtained from a list of numbers
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = x
sumDigits (x:xs) = ((x `div` 10) + x `mod` 10) + sumDigits (xs)


validate :: Integer -> Bool
--validate (creditCardNum) = toDigits (creditCardNum)
validate cardnum
 | ((cardnum) `mod` 10) == 0 = True
 | ((cardnum) `mod` 10) < 0 = False
 | ((cardnum) `mod` 10) > 0 = False
 | otherwise = False

main :: IO()
main = do
--  print (toDigits (4012888888881881))
  print (validate(sumDigits(doubleEveryOther(toDigits (4012888888881881)))))
  print (validate(sumDigits(doubleEveryOther(toDigits (4012888888881882)))))

--  print(doubleEveryOther [4,0,1,2,8,8,8,8,8,8,8,8,1,8,8,1])
--  print(sumDigits [8,0,2,2,16,8,16,8,16,8,16,8,2,8,16,1])
--  print(validate (90))
--  print (toDigits  (4012888888881882))
--  print (toDigits num)
--  print (toDigitsRev num)
--  print (doubleEveryOther (toDigits(num)))

--num = 4012888888881881
--num = 4012888888881882
