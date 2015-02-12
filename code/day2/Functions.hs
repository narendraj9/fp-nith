--------------------------------------------------------------------------------
-- Functions in Haskell
--------------------------------------------------------------------------------
myhead :: [a] -> a
myhead [] = error "I have no head."
myhead (x:xs) = x


mytail :: [a] -> [a]
mytail [] = error "I have no tail."
mytail (x:xs) = xs


betterSum :: Num a => [a] -> a
betterSum xs = helper xs 0
  where helper [] acc = acc
        helper (x:xs) acc = helper xs $! acc

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = x `f` myreverse xs
                   where { f x ys = ys ++ [x];
                           g y = y;
                         }


--------------------------------------------------------------------------------
-- Chase the pattern!
--------------------------------------------------------------------------------

doubleEach :: Num a => [a] -> [a]
doubleEach [] = []
doubleEach (x:xs) = (2*x) : doubleEach xs


squareEach :: Num a => [a] -> [a]
squareEach [] = []
squareEach (x:xs) = (x*x) : squareEach xs

---
-- [x1, x2, x3, ... xn]
-- [ f x1, f x2, f x3, .. f xn]
--
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

--------------------------------------------------------------------------------
-- List comprehensions
--------------------------------------------------------------------------------
-- {x*x : x belongs to {1,2,3,4}}
-- {1,4,9,16}
squares = [x*x | x <- [1,2,3,4]]


quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort smallerThanX) ++ [x] ++ (quicksort greaterThanX)
  where smallerThanX = [p | p <- xs, p < x]
        greaterThanX = [q | q <- xs, q >= x]

--------------------------------------------------------------------------------
-- Chase the pattern
--------------------------------------------------------------------------------

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs


myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

--------------------------------------------------------------------------------
-- Let's make folding a list an operation that we are aware of. A more general
-- operation than adding or finding the product of the elements.
--------------------------------------------------------------------------------
foldIt :: (a -> b -> b) -> b -> [a] -> b
foldIt f init [] = init
foldIt f init (x:xs) = x `f` (foldIt f init xs)

--------------------------------------------------------------------------------
-- foldIt (+) 0 [1,2,3]
-- 1 + (foldIt (+) 0 [2,3])
-- 1 + (2 + (3 + foldIt (+) 0 []))
-- 1 + (2 + (3 + 0)))
-- 1 : (2 : (3 : []))
--------------------------------------------------------------------------------

{-
foldr f init [x1, x2, x3..xn]
foldr f init (x1 : (x2 : (x3 ... xn :[])))
x1 `f` (x2 `f` (x3 `f` ... xn `f` init))

--}


--------------------------------------------------------------------------------
-- Chase the pattens! And climb up the Abstraction ladder. That's Life.
--------------------------------------------------------------------------------

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
    putStrLn $ show (betterSum [1..1000000000])


--------------------------------------------------------------------------------
-- Typeclasses
--------------------------------------------------------------------------------
{-
Num
Ord
Eq
Show
-}

data Color = Red | Green | Blue

instance Show Color where
  show Red = "Laal"
  show Blue = "Neela"
  show Green = "Hara"

instance Eq Color where
  Red == Red = True
  Blue == Blue = True
  Green == Green = True
  _ == _ = False

instance Ord Color where
  compare Blue Red = LT
  compare Blue Green = LT
  compare Green Red = LT
  compare Blue Blue = EQ
  compare Green Green = EQ
  compare Red Red = EQ
  compare _ _ = GT

data Truth = MyRight | MyWrong deriving (Show, Eq)

data List a = Nil | a :+ (List a)

instance Show a => Show (List a) where
  show Nil = "[]"
  show (x :+ xs) = "[" ++ show x ++ restOfString
                        where restOfString = case xs of
                                                Nil -> "]"
                                                _ -> "," ++ tail (show xs)


--------------------------------------------------------------------------------
-- Complex Arithmetic
--------------------------------------------------------------------------------
data Complex a = PlusI a a

instance (Eq a) => Eq (Complex a) where
  (x `PlusI` y) == (p `PlusI` q) = (x == p) && (y == q)

instance Show a => Show (Complex a) where
  show (x `PlusI` y) = show x ++ "+" ++ show y ++ "i"

instance Num a => Num (Complex a) where
  (x `PlusI` y) + (p `PlusI` q) = (x+p) `PlusI` (y+q)

  (x `PlusI` y) * (p `PlusI` q) = (x*p - y*q) `PlusI` (x*q + y*p)

  abs (x `PlusI` y) = (x*x + y*y) `PlusI` 0

  signum x = error "We don't know what that means."
  fromInteger x = (fromInteger x) `PlusI` (fromInteger 0)
  negate (x `PlusI` y) = (-x) `PlusI` (-y)
