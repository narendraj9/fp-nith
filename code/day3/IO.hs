--------------------------------------------------------------------------------
-- IO in Hasell
--------------------------------------------------------------------------------


saySomething :: IO ()
saySomething = putStrLn "Hey there!"

sayHello :: IO ()
sayHello = putStrLn "Hello"


anewRecipe = (saySomething >> sayHello)

{-
let x = printf("Say Something!"); in
  x ; x;

-- Haskell
let x = putStrLn "Say Something" in
   x ; x;

-}


{-
  recipe1 String >>= (\name -> newRecipe)

-}
main = do {
  putStrLn "Hey! What's your name?" >>
  getLine >>= \name ->
  putStrLn ("Hi " ++ name);
}
