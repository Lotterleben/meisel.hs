module Iter where
-- gehe davon aus dass xEnd das maximum für x ist...

main = print (euler 0.001 0.2)

f' x y = 10 - 500 * y + 500 * x

euler h x_end = y_next 1 h 0 x_end []

y_next:: Double -> Double -> Double -> Double -> [Double] -> [Double]
y_next y h t_n x_end ys
    | t_n == x_end = ys -- rekursionsanker
    | otherwise    = y_next f'_curr h t_next x_end (f'_curr:ys)
        where t_next  = t_n+h
              f'_curr = y+h * (f' t_n y)
