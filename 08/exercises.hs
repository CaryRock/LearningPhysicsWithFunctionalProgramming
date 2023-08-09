import Graphics.Gnuplot.Simple

type R = Double

integral :: R -> (R -> R) -> R -> R -> R
integral dt f a b
    = sum [f t * dt | t <- [a + dt/2, a + 3*dt/2 .. b - dt/2]]

xRange :: [Double]
xRange = [0, 0.01 .. 10]

--plot1 :: IO ()
--plot1 = plotFunc [] [0, 0.01 .. 10] cos
plot2 = plotFunc[] xRange cos
-- equivalent: plot1 = plotFunc [] [0, 0.01 .. 10 :: Double] cos <- tells GHC 
--                              that the last element, '10', is of type Double,
--                              meaning they're all of type Double
