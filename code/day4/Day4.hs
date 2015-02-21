--------------------------------------------------------------------------------
-- Day 4 Code
--------------------------------------------------------------------------------
import OldDefs.Day3 hiding (getName, getSalary)


class Listable a where
  toList :: a -> [a]


--------------------------------------------------------------------------------
-- Functor
--------------------------------------------------------------------------------
instance Functor Tree where
  fmap = treeMap

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons v restOfList) = (Cons (f v) (fmap f restOfList))

-- instance Functor ((->) e) where
--   fmap = (.)

alist = foldr Cons Nil [1,2,3,4]


data Employee = Employee { getName :: String
                         , getPhone :: String
                         , getSalary :: Double
                         } deriving Show

nj = Employee "Narendra" "988222" 99999999999999

calcTax :: Double -> Double
calcTax income = (income * 1/10)
