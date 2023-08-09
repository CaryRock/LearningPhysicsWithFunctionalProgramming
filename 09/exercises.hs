import Graphics.Gloss

type R = Double

-- The syntax to call this one is pythag (x, y)
pythag :: (R, R) -> R   -- Takes a tuple of Rs and returns an R
pythag (x, y) = sqrt(x**2 + y**2)

-- The syntax to call this one is pythagCurried x y
pythagCurried :: R -> R -> R
pythagCurried x y = sqrt (x**2 + y**2)

-- The above two functions are exclusive to their inputs: calling
-- pythagCurried on (x, y) will yield an error. Ditto, pythag x y
-- The way to work around this is to use the built-in function "curry" to
-- curry the pythag function - this tells it to change the the type from
-- (R, R) -> R to R -> R -> R.
-- Likewise, there's also "uncurry" which takes R -> R -> R to (R, R) -> R
-- Check ":t curry" and ":t uncurry" for more information

displayMode :: Display
displayMode = InWindow "Axes" (1000, 700) (10, 10)

disk :: Float -> Picture
disk radius = ThickCircle (radius / 2) radius

redDisk :: Picture
redDisk = Color red (disk 25)

projectileMotion :: Float -> Picture
projectileMotion t = Translate (xDisk t) (yDisk t) redDisk

xDisk :: Float -> Float
xDisk t = 40 * t

yDisk :: Float -> Float
yDisk t = 80 * t - 4.9 * t**2

main :: IO ()
main = animate displayMode black projectileMotion
