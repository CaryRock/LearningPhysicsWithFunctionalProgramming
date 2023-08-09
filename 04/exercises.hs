type R = Double     -- type "Real" is a Double

type Time           = R
type TimeInterval   = R
type Position       = R
type Velocity       = R
type Acceleration   = R

type PositionFunction = Time -> Position
type VelocityFunction = Time -> Velocity

type Derivative = (R -> R) -> R -> R    -- Takes a f(R) and some R, then returns an R




derivative :: R -> Derivative   -- Whole type: R -> (R -> R) -> R -> R
-- dt : R       x: (R -> R)     t: R; returns an R
derivative dt x t = (x (t + dt/2) - x (t - dt/2)) / dt

carPosition :: Time -> Position -- equivalently, 'PositionFunction"
carPosition t = cos t

carVelocity :: Time -> Velocity
carVelocity = derivative 0.001 carPosition

carVelocity' :: Time -> Velocity
carVelocity' t = -sin t

velFromPos :: R                     -- dt
            -> (Time -> Position)   -- position function
            -> (Time -> Velocity)   -- velocity function
velFromPos dt x = derivative dt x

posConstVel :: Position -> Velocity -> Time -> Position
posConstVel x0 v0 t = v0 * t + x0

accFromVel  :: R                        -- dt
            -> (Time -> Velocity)       -- velocity function
            -> (Time -> Acceleration)   -- acceleration function
accFromVel = derivative

velConstAcc :: Velocity -> Acceleration -> Time -> Velocity
velConstAcc v0 a0 t = a0 * t + v0

positionCA :: Position -> Velocity -> Acceleration -> Time -> Position
positionCA x0 v0 a0 t = a0 ** 2 / 2 + v0 * t + x0

cube :: R -> R
cube x = x**3

cube' :: R -> R
cube' x = 3 * x**2


x20 = [1.0, 1.05..2.0]

compareEps ::   R -> R -> R    -- epsilon in, differences out
compareEps e x1 =
    let y20 = derivative e cube x1
        y20exact = cube' x1
    in  y20exact - y20
