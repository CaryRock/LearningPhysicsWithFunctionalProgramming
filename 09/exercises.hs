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

-- Ex. 1
polarToCart :: (R, R) -> (R, R)
-- Takes as input (r, theta), theta in radians -> (x, y) Cartesian
polarToCart (r, t) = (r * cos t, r * sin t)

-- Ex. 2
-- See above explanation on curry/uncurry

-- Ex. 3
headSafe :: [a] -> Maybe a
headSafe [] = Nothing
headSafe (x:_) = Just x

-- Ex. 4
maybeToList :: Maybe a -> [a]
maybeToList x = case x of
    Nothing     -> []
    Just x      -> [x]

-- Ex. 5
-- When two lists with different lenghts are zipped together, the longer one is 
-- truncated to the length of the shorter. 

-- Ex. 6
zip' :: ([a], [b]) -> [(a,b)] -- Use curry or uncurry
zip' (xs, ys) = [ (x, y) | 

delineate :: IO ()
delineate = putStrLn ""

main :: IO ()
main = do
    delineate
    putStrLn "Exercise 1"
    putStrLn . show $ polarToCart (5, 2)

    delineate
    putStrLn "Exercise 2"
    putStrLn "In code file."
    
    delineate
    putStrLn "Exercise 3"
    putStrLn . show $ headSafe [4]
    putStrLn . show $ (if headSafe [] /= Just 4 then "Nothing!" else "Incorrect!")

    delineate
    putStrLn "Exercise 4"
    putStrLn . show . maybeToList $ headSafe [4]
    putStrLn . show . maybeToList $ headSafe ["Nothing would be here"]

    delineate
    putStrLn "Exercise 5"
    
