{-# LANGUAGE BangPatterns #-}
module Main where
import Meisel
import Iterationsgleichungen
import Graphics.Gloss

-- main = print (startposition 1 2)
--import Graphics.Rendering.Chart.Easy
--import Graphics.Rendering.Chart.Backend.Diagrams

--circle :: [(Double,Double)]
--circle = [ (r a * sin (a*dr),r a * cos (a*dr)) | a <- [0,0.5..360::Double] ]
--  where
--    dr = 2 * pi / 360
--    r a = 0.8 * cos (a * 20 * pi /360)

--main = toFile def "plots/test.svg" $ do
--    layout_title .= ""
--    plot (line "euler" [euler_with_index 0.001 0.2])
--    plot (line "rk2" [rk2_with_index 0.001 0.2])

--main :: IO ()
--main
-- =  animate (InWindow "Zen" (800, 600) (5, 5))
--        white
--        frame


--v_x = 10
--v_y = -10

--frame :: Float -> Picture
--frame timeS = (Circle (10 + (timeS*10)))


--- main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Data.Color
import GHC.Float


type Time = Float
type Size = Float
type Position = (Double, Double)
type Trail = [Position]
type Index = Int
type StepWidth = Double
type Model = (Size, Trail, Index, StepWidth, Time)


main = do
  let dispWin = InWindow "Onset Test" (1440, 600) (0,0)

  let stepWidth = 0.001
  let start = (10.0, reverse(foo_y2_next_e stepWidth 120 0 0 200 []), 0, stepWidth, 0)

  simulate
    dispWin
    white
    45
    start
    draw
    step

draw :: Model -> Picture
draw (size, trail, index, stepWidth, time) =
  Pictures
  [ drawMoving (trail!!index)
  , color black $ line [(-99999.0, 0), (999999.0, 0)]
  , color black $ Scale 0.25 0.25 $ Text (show time)]

drawMoving :: Position -> Picture
drawMoving (x, y) =
  color red
  $ (translate (double2Float x) (double2Float y))
  $ circleSolid 10.0



step :: ViewPort -> Time -> Model -> Model
step v t (size, trail, index, stepWidth, time)
  | t_next > (length trail) = (size, trail, index, stepWidth, time)
  | otherwise               = (size, trail, t_next, stepWidth, time+t)
  where t_next = index + floor((t / (double2Float stepWidth)))

