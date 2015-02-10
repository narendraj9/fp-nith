--------------------------------------------------------------------------------
-- Workshop Practice | [This is my file. I practised.]
--------------------------------------------------------------------------------
import Control.Applicative


-- a tree with data in the nodes
data Tree a = Empty | Node (Tree a) a (Tree a)

{-

How to print the tree?

0: root
  1: child_one
    2: child_of_child_one
    2: child_of_child_two
  1: child_two
  1: child_three

-}

instance Show a => Show (Tree a) where
  show Empty = "."
  show tree = helper 0 tree
    where helper i Empty = ""
          helper i (Node l v r) = replicate (2*i) ' ' ++ (show i) ++ ": " ++ show v ++ "\n" ++
                                  helper (i+1) l ++ "\n" ++
                                  helper (i+1) r

--------------------------------------------------------------------------------
-- Chase the pattern
--------------------------------------------------------------------------------
treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

treeSum :: Num a => Tree a -> a
treeSum Empty = 0
treeSum (Node l v r) = v + treeSum l + treeSum r

treeDepth :: Tree a -> Int
treeDepth Empty = 0
treeDepth (Node l _ r) = 1 + (treeDepth l) `max` (treeDepth r)


flatten :: Tree a -> [a]
flatten Empty = []
flatten (Node l v r) = flatten l ++ [v] ++ flatten r


--
treeFold :: (b -> a -> b -> b) -> b -> Tree a -> b
treeFold f init (Empty) = init
treeFold f init (Node leftTree x rightTree) = f (treeFold f init leftTree) x (treeFold f init rightTree)


treeDepth' :: Tree a -> Int
treeDepth' = (treeFold (\ldepth _ rdepth -> 1 + max ldepth rdepth) 0)

treeSum' :: Num a => Tree a -> a
treeSum' = treeFold (\lsum x rsum -> lsum + x + rsum) 0

squareRoot :: Double -> Double
squareRoot = (**(1/2))

--------------------------------------------------------------------------------
-- Yet another typeclass : Monoid
--------------------------------------------------------------------------------
main :: IO ()
main = putStr "Enter a number: " >> (readLn >>= (\n -> putStrLn ("The square root of " ++ show n ++ " is " ++ show (squareRoot n))))


--------------------------------------------------------------------------------
-- Record syntax
--------------------------------------------------------------------------------
data Employee = EmployeeRecord { name :: String
                               , address :: String
                               , salary :: Double
                               , ident :: String
                               }
              deriving Show


-- My lists with a pairing Applicative instances
-- Default lists [a] have the possibilities context based
-- Applicative instance.
data List a = Nil | Cons a (List a) deriving Show

instance Functor List where
  fmap _ (Nil) = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  (Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs)
  _ <*> _ = Nil

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)
