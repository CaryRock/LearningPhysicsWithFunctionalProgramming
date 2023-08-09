import Graphics.Gnuplot.Simple

type R  = Double
-- Use:
-- plotFunc [] [0, 0.1 .. 10] cos
-- command [attributes - titles, etc.] [data range] function

square :: Double -> Double
square x = x**2

plot1 :: IO ()
plot1 = plotFunc [] [-3, -2.99 .. 3] square

-- 7.1
plot71 :: IO ()
plot71 = plotFunc [] [-10, -9.95 .. 10] sin

-- 7.2
yRock :: R -> R -> R
yRock v0 t = v0 - 9.8 * t

plot72 :: IO ()
plot72 = plotFunc [] [0, 0.1 .. 6] $ yRock 30

-- 7.3
plot73 :: IO ()
plot73 = plotFunc [] [0, 0.1 .. 4] $ yRock 20
