module Iterationsgleichungen where
-- gehe davon aus dass xEnd das maximum für x ist...

-- main = print (euler 0.001 0.2)

f' x y = 10 - 500 * y + 500 * x

--------------------------------------------------------------------------------
--- foobar h x_end = zipWith [0..] (euler h x_end)

euler_with_index :: Double -> Double -> [(Double, Double)]
euler_with_index h x_end = [(a * h, b) | (a, b) <- xs]
  where xs = zip [0..] (euler h x_end)

euler h x_end = reverse (y_next_e 1 h 0 x_end [])

y_next_e:: Double -> Double -> Double -> Double -> [Double] -> [Double]
y_next_e y h t_n x_end ys
    | t_n >= x_end = ys -- rekursionsanker
    | otherwise    = y_next_e f'_curr h t_next x_end (f'_curr:ys)
        where t_next  = t_n+h
              f'_curr = y+h * (f' t_n y)
--------------------------------------------------------------------------------

-- turn a list of y coordinates into a list of (x,y) coordinates.
-- h:     step width
-- ys:    list of y coordinates
add_x_coordinates:: (Ord a, Num a, Enum a) => a -> [a] -> [(a,a)]
add_x_coordinates h ys = [(a * h,b) | (a,b) <- (zip [0..] ys)]

-- Euler auf (zerlegte) DGLn 2. Ordnung

-- h:         step width
-- f:         function to apply
-- y1 and y2: start values (TODO: better names)
-- x_end:     last x val to calculate y for
-- returns:   a list of (x,y) coordinates
-- (concat and finally reverse for performance reasons)
euler_2o :: (Ord a, Num a, Enum a) => a -> (a -> a -> a) -> a -> a -> a -> [(a,a)]
euler_2o h f y1 y2 x_end = reverse(y2_next_e h f y1 y2 0 x_end [])

-- h:     step width
-- f:     function to apply
-- y1:    val of previous y1 iteration (initially y(0) = y1)
-- y2:    val of previous y2 iteration (initially y'(0) = y2)
-- t_n:   step tracker
-- x_end: last x val to calculate y for
-- ys:    list of y coordinate results
-- returns:   a list of (x,y) coordinates
y2_next_e :: (Ord a, Num a, Enum a) => a -> (a->a->a) -> a -> a -> a -> a -> [a] -> [(a,a)]
y2_next_e h f y1 y2 t_n x_end ys
    | t_n >= x_end =  add_x_coordinates h ys-- rekursionsanker
    | otherwise    = y2_next_e h f y1_next y2_next t_next x_end (y1:ys)
        where y1_next = y1 + h * (f t_n y1)
              y2_next = y2 + h * y1 -- beware, for this is hardly generic (TODO)
              t_next  = t_n+ h

{-
y2_next_e h f y1 y2 t_n x_end ys
    | t_n >= x_end = ys -- rekursionsanker
    | otherwise    = y2_next_e h f y1_next y2_next t_next x_end (y2_next_e:ys)
        where t_next  = t_n+h
              y1_next = y1 + h*(f t_n y2)
              y2_next = y2 + h*y1
-}

--------------------------------------------------------------------------------


rk2_with_index :: Double -> Double -> [(Double, Double)]
rk2_with_index h x_end = [(a * h, b) | (a, b) <- xs]
  where xs = zip [0..] (rk2 h x_end)


rk2 h x_end = reverse (y_next_rk 1 h 0 x_end [])

y_next_rk:: Double -> Double -> Double -> Double -> [Double] -> [Double]
y_next_rk y h t_n x_end ys
    | t_n >= x_end = ys -- rekursionsanker
    | otherwise    = y_next_rk f'_curr h t_next x_end (f'_curr:ys)
        where f'_curr = (y+k2)
              t_next  = t_n+h
              k1 = h * (f' t_n y)
              k2 = h * (f' (t_n+ (h/2)) (y + (k1/2)))
