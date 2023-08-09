type R = Double

-- 5.1
numbers :: [R]
numbers = [-2.0, -1.2..2.0]

-- 5.2
sndItem0 :: [a] -> a
sndItem0 []     = error "Empty list has no second element"
sndItem0 (x:[]) = error "1-item list has no second element"
sndItem0 (x:xs:_) = xs

-- 5.3
-- type of 'length "Hello, world!"' = Foldable t => t a -> Int
-- the type 't' is a Foldable type that acts on some type a to return an Int

-- 5.4
exercise54 :: Int -> [Int]
exercise54 0 = [-1, 0, 1]
exercise54 x = [-2 * (abs x)..2*(abs x)]
-- This function takes an integer and constructs a list from -2*abs(x) to 
-- 2*abs(x), meaning 0 is in the center. If x = 0, returns [-1, 0, 1].

-- 5.5
null' :: Foldable t => t a -> Bool
null' x 
    | length x == 0 = True
    | otherwise     = False

-- 5.6
last' :: [a] -> a
last' [x] = x
last' (_:xs) = head $ reverse xs

-- 5.7
palindrome :: String -> Bool
palindrome x
    | reverse x == x= True
    | otherwise     = False

-- 5.9
cycle' :: [a] -> [a]
cycle' = concat . repeat 

-- 5.11
-- If the final element is greater than or equal to half the step between 
-- elements, the final element in the generated list will the (over)step of 
-- the end. I.e., [0.0, 3.0..8.0] generates up to 9.0 because 8.0 >= 7.5, but 
-- [0.0, 3.0..7.49] stops at 6.0 because 7.49 is under that half-step size.

-- 5.12
foldl' f z []       =   z
foldl' f z (x:xs)   =   let z' = z `f` x
                        in seq z' $ foldl' f z' xs

-- Riemann-Zeta (s = 2); shown by Euler in the 1730s
invSquare :: Int -> Double
invSquare 0 = error "Cannot divide by 0"
invSquare x = (fromIntegral x)**(-2)

riemannZeta :: Int -> Double    -- Takes an upper bound on the sum, returns the sum
riemannZeta 1 = 1
riemannZeta n = 
    let x = [1..n]
        y = map invSquare x
    in foldl' (+) 0 y

-- 5.13
factorial :: Integer -> Integer -- Takes the factorial of a number
factorial 0 = 1
factorial 1 = 1
factorial n = product [1..n]

-- 5.14
expList :: R -> [R] -- Takes in a real number, x, for input and then cmoputes an 
                    -- infinite list of successive approximations to exp(x)
expList x = let n = [1.0..]
            in  map (\m -> (1 + x/m)**m) n
-- To get within 1% for x = 1 (i.e., exp(1)), need m = 5
-- To get within 1% for x = 10, need m = 468 iterations

expTerm :: R -> Integer -> R   -- Returns the value of the nth term of the expon. series
expTerm x n = x ** (fromIntegral n) / (fromIntegral (factorial n))

expSeries :: R -> [R]   -- Takes in a real x and computes an infinite list of
                        -- applications of the series approximation of exp(x)
expSeries x = [sum . map (expTerm x) $ [0..m] | m <- [0..], True]
    --sum . map (expTerm x) $ [0..] -- produces R, not [R]

-- Type system doesn't care about the intermediates, just the inputs
calcExpSeriesDiff :: R -> Int -> R
calcExpSeriesDiff x n = let y = last . take n . expSeries $ x
                            ey= exp(x)
                        in ey - y
-- To get within 1% for x = 1, need 5 terms
-- To get within 1% for x = 10, need 30 terms 
