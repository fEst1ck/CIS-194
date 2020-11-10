-- course homepage https://www.seas.upenn.edu/~cis194/spring13/
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | n < 10 = [n]
    | otherwise = n `mod` 10 : (toDigitsRev $ n `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev l@[_] = l
doubleEveryOtherRev (x:(y:zs)) = x : (2 * y : doubleEveryOtherRev zs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherRev . reverse

sumDigits :: [Integer] -> Integer
sumDigits ns = sum $ map (sum . toDigits) ns

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 p1 p2 _ = [(p1, p2)]
hanoi n p1 p2 p3 = hanoi (n - 1) p1 p3 p2 ++ [(p1, p2)] ++ hanoi (n - 1) p3 p2 p1