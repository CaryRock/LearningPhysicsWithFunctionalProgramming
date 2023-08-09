heavisideX :: Double -> Double
heavisideX x
    | x <= 0 = 0
    | otherwise = x

rOrInverseR2 :: Double -> Double
rOrInverseR2 r
    | r <= 1 = r
    | otherwise = 1/(r**2)

isXorY :: Char -> Bool
isXorY c
    | c == 'X' || c == 'Y' = True
    | otherwise = False

bagFee :: Bool -> Int
bagFee checking =   if checking
                    then 100
                    else 0

bagFee2 :: Bool -> Int
bagFee2 True = 100
bagFee2 False = 0

amazingCurve :: Int -> Int
amazingCurve score
    | x > 100 = 100
    | otherwise = x
    where x = 2 * score

circleRadius :: Double
circleRadius = 3.5

cot :: Double -> Double
cot x = 1 / tan x

fe :: Double -> Double
fe epsilon = epsilon * tan ( epsilon * pi / 2)

fo :: Double -> Double
fo epsilon = -epsilon * cot (epsilon * pi / 2)

g :: Double -> Double -> Double
g nu epsilon = sqrt (nu**2 - epsilon**2)
