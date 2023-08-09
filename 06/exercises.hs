type R = Double

type Time           = R
type TimeInterval   = R
type Position       = R
type Velocity       = R
type Acceleration   = R

type PositionFunction = Time -> Position
type VelocityFunction = Time -> Velocity

type Integration = (R -> R)     -- function
                 -> R           -- lower limit
                 -> R           -- upper limit
                 -> R           -- result

type AntiDerivative = R         -- initial value
                    -> (R -> R) -- function
                    -> (R -> R) -- antiderivative of said function


integral :: R -> Integration
integral dt f a b   -- integration in terms of the midpoint rule
    = sum [f t * dt | t <- [a + dt/2, a + 3*dt/2 .. b - dt / 2]]

-- Defining an antiderivative in terms of an integral
-- Returns F(x) = F(0) + int dt a _0 ^a
antiDerivative :: R -> AntiDerivative
antiDerivative dt v0 a t = v0 + integral dt a 0 t

velFromAcc :: R                         -- dt
            -> Velocity                 -- initial velocity
            -> (Time -> Acceleration)   -- acceleration function
            -> (Time -> Velocity)       -- velocity function
velFromAcc dt v0 a t = antiDerivative dt v0 a t

posFromVel :: R                     -- dt
            -> Position             -- initial position
            -> (Time -> Velocity)   -- velocity function
            -> (Time -> Position)   -- position function
posFromVel = antiDerivative

integralN :: Int -> Integration
integralN n f a b
    =   let dt = (b - a) / fromIntegral n
        in integral dt f a b

-- Equivalent, to emphasize this point
integralN' :: Int -> Integration
integralN' n f a b
    = integral ((b - a) / fromIntegral n) f a b

-- 6.1
yRock :: R -> R -> R
yRock v0 t = v0 * t

vRock :: R -> R -> R
vRock v0 t = v0 - 9.8 * t

-- 6.2
-- :t take 4 :: [a] -> [a]

-- 6.3
-- :t not :: Bool -> Bool
-- `not` can be the first argument to map; the type is of [Bool] -> [Bool]
-- -> type a == type b is allowed

-- 6.4
greaterThanOrEq7' :: Int -> Bool
greaterThanOrEq7' n = n >= 7

-- 6.5
exer65 :: Int -> String -> Bool
exer65 n s = length s >= n
-- This function takes in an integer and a string and compares the length of S
-- to the value of n - returns true if length S >= n

-- 6.6
exer66 :: [a] -> Bool
exer66 x = length x > 6

-- 6.7
-- A string is a list of characters.

-- 6.8
square :: Double -> Double
square x = x**2

squareList :: [Double] -> [Double]
squareList x = map square x

-- 6.9
repeat' :: a -> [a]
repeat' = iterate id

-- 6.10
replicate' :: Int -> a -> [a]
replicate' n x = take n $ repeat x

-- 6.11
-- v0 = 0, a0 = 5 m/s
constAcc5from0 :: [Int]
constAcc5from0 = [x | x <- [0,5..]]

-- 6.12
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

-- 6.13
filter' :: (a -> Bool) -> [a] -> [a]
filter' pred x = [i | i <- x, pred i]

-- 6.14
average :: [R] -> R
average [x] = x
average xs = (sum xs) / (fromIntegral $ length xs)

-- 6.16
trapIntegrate   :: Int      -- # trapezoids n
                -> (R -> R) -- function f
                -> R        -- lower limit a
                -> R        -- upper limit b
                -> R        -- result
trapIntegrate n f a b = 
    let dx = (b - a)/ fromIntegral n
        x = [a, a + dx .. b]
        y = map f x
        diff = [head y, last y]
    in  dx * (sum y - (sum diff) / 2)
