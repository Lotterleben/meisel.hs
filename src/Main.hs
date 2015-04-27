module Main where
import Meisel
import Iterationsgleichungen

-- main = print (startposition 1 2)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

circle :: [(Double,Double)]
circle = [ (r a * sin (a*dr),r a * cos (a*dr)) | a <- [0,0.5..360::Double] ]
  where
    dr = 2 * pi / 360
    r a = 0.8 * cos (a * 20 * pi /360)

main = toFile def "example7_big.svg" $ do
    layout_title .= "Parametric Plot"
    plot (line "euler" [euler_with_index 0.001 0.2])
    plot (line "rk2" [rk2_with_index 0.001 0.2])
    --- plot (line "foobar1" [euler_with_index 0.0001 0.2])

