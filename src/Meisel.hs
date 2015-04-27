module Meisel where

erdradius:: Double
erdradius = 6378             -- km
erdmasse :: Double
erdmasse = 5.9736*10**24     -- kg
gravitation :: Double
gravitation = 66.743*10**(-12) -- m^3/(kg*s^2)
gravitation_constant :: Double
gravitation_constant = 9.81  -- m/(s^2)


data Vector2D = Vector2D Double Double deriving (Show)

startposition :: Double -> Double -> Vector2D
startposition winkel hoehe = Vector2D (cos(winkel)*gesamthoehe) (sin(winkel)*gesamthoehe)
    where gesamthoehe = erdradius + hoehe


fall :: (Num a) => a -> a -> [a]
fall height obj_weight = TODO