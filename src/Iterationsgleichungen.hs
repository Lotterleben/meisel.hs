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
-- Euler auf (zerlegte) DGLn 2. Ordnung

-- h:  step width
-- f: function to apply
-- y1 and y2: start values (TODO: better names)
-- x_end last x val to calculate y for
-- (concat and finally reverse for performance reasons)
-- TODO: ist t_n.init = y2 richtig?
euler_2o :: (Ord a, Num a) => a -> (a -> a -> a) -> a -> a -> a -> [a]
euler_2o h f y1 y2 x_end = reverse(y2_next_e h f y1 y2 0 x_end [])

-- y1:    val of previous y1 iteration (initially y(0) = y1)
-- y2:    val of previous y2 iteration (initially y'(0) = y2)
-- f:     function to apply
-- h:     step width
-- t_n:   step tracker
-- x_end: last y val to calculate
-- ys:    list of final results
y2_next_e :: (Ord a, Num a) => a -> (a->a->a) -> a -> a -> a -> a -> [a] -> [a]
y2_next_e h f y1 y2 t_n x_end ys
    | t_n >= x_end = ys -- rekursionsanker
    | otherwise    = y2_next_e h f y1_next y2_next t_next x_end (y2_next:ys)
        where t_next  = t_n+h
              y1_next = y1 + h*(f t_n y2)
              y2_next = y2 + h*y1

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
