--------------------------------------------------------------------------------
-- Functions in Haskell
--------------------------------------------------------------------------------
myhead :: [a] -> a
myhead [] = error "I have no head."
myhead (x:xs) = x


mytail :: [a] -> [a]
mytail [] = error "I have no tail."
mytail (x:xs) = xs

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

betterSum :: Num a => [a] -> a
betterSum xs = helper xs 0
  where helper [] acc = acc
        helper (x:xs) acc = helper xs (seq acc (x + acc))

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

someFunkyFunction :: [a] -> [a] -> [a]
someFunkyFunction = error "Will define it later"


addTwo :: Num a => a -> a -> a
x `addTwo` y = x + y

squareRoot :: Double -> Double
squareRoot n = let helper guess = if isGoodEnough guess
                                  then guess
                                  else helper (improve guess)
                   isGoodEnough g = abs (g*g - n) / n < 0.01
                   improve g = seq g (g + n / g) / 2
               in helper 1
{-
double :: Integer -> Integer
double x = 2 * x
--}


main :: IO ()
main = do 
            line <- getLine
            let number = read line :: Double
            putStrLn $ show (squareRoot number)

