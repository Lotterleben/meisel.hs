module Iter where
-- gehe davon aus dass xEnd das maximum für x ist...

main = print (euler 0.001 0.2)

f' x y = 10 - 500 * y + 500 * x

--------------------------------------------------------------------------------
euler h x_end = y_next_e 1 h 0 x_end []

y_next_e:: Double -> Double -> Double -> Double -> [Double] -> [Double]
y_next_e y h t_n x_end ys
    | t_n == x_end = ys -- rekursionsanker
    | otherwise    = y_next_e f'_curr h t_next x_end (f'_curr:ys)
        where t_next  = t_n+h
              f'_curr = y+h * (f' t_n y)
--------------------------------------------------------------------------------

rk2 h x_end = y_next_rk 1 h 0 x_end []

y_next_rk:: Double -> Double -> Double -> Double -> [Double] -> [Double]
y_next_rk y h t_n x_end ys
    | t_n == x_end = ys -- rekursionsanker
    | otherwise    = y_next_rk f'_curr h t_next x_end (f'_curr:ys)
        where f'_curr = (y+k2)
              t_next  = t_n+h
              k1 = h * (f' t_n y)
              k2 = h * (f' (t_n+ (h/2)) (y + (k1/2)))
