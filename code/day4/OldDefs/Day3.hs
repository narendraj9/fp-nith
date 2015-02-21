--------------------------------------------------------------------------------
-- Day 3
--------------------------------------------------------------------------------
module OldDefs.Day3 where

data Color = Blue | Green | Red deriving (Show, Eq)
data List a = Nil | Cons a (List a) deriving Show
data Complex a = a `PlusI` a

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Prelude data declarations
--------------------------------------------------------------------------------
myheadBetter :: [a] -> Maybe a
myheadBetter (x:xs) = (Just x)
myheadBetter [] = Nothing


tellMeAboutYourHead :: Show a => [a] -> String
tellMeAboutYourHead xs = case (myheadBetter xs) of
                          Nothing -> "I don't have a head."
                          (Just x) -> "My head is " ++ show x

--------------------------------------------------------------------------------
-- Record Syntax
--------------------------------------------------------------------------------
type Name = String
type Address = String
type Salary = Double
type ID = String

data Person = PersonRecord ID Name Address Salary deriving Show

data PersonB = PersonB { getID :: ID
                       , getName :: Name
                       , getAddress :: Address
                       , getSalary :: Salary
                       } deriving Show

--
defaultPerson = PersonB { getID = "Nil"
                        , getAddress = "Nil"
                        , getName = "Nil"
                        , getSalary = 0.0
                        }
--
myGetName (PersonB { getName = name }) = name


--------------------------------------------------------------------------------
-- Functions on trees
--------------------------------------------------------------------------------
data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show
atree = Node 32 (Leaf 24)  (Node 144 (Leaf 12) (Leaf 13))

treeSize :: Tree a -> Int
treeSize (Leaf _) = 1
treeSize (Node _ leftTree rightTree) = 1 + treeSize leftTree + treeSize rightTree

treeDepth :: Tree a -> Int
treeDepth (Leaf _) = 1
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)


treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Node x left right) = Node (f x) leftMapped rightMapped
                      where leftMapped = (treeMap f left)
                            rightMapped = (treeMap f right)


treeFlatten :: Tree a -> [a]
treeFlatten (Leaf x) = [x]
treeFlatten (Node x left right) = (treeFlatten left) ++ [x] ++ (treeFlatten right)
