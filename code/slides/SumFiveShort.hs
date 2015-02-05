-- Sum up the *sequence* 1 to 5, inclusive
sum [1..5]

-- Definition of the sum function
sum [] = 0
sum (x:xs) = x + sum xs
