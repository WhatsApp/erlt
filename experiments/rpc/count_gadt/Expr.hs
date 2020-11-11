-- This shows an example of GADTs in Haskell.
-- Copy this file to coderpad.io to play around with it.

{-# LANGUAGE GADTs #-}

data Expr r where                                     -- -type expr(Result) =
    I   :: Int  -> Expr Int                           -- {i, integer{})
    B   :: Bool -> Expr Bool                          -- {b, boolean()}
    S :: String -> Expr String
    Add :: Expr Int -> Expr Int -> Expr Int           -- {add, expr(integer()), expr(integer())}
    Mul :: Expr Int -> Expr Int -> Expr Int           -- {mul, expr(integer()), expr(integer())}
    Concat :: Expr String -> Expr String -> Expr String
    Equal :: (Eq r) => Expr r -> Expr r -> Expr Bool    -- {equal, expr(E), expr(E)}

eval :: Expr r -> r          -- -spec eval(expr(R)) -> R
eval (I i) = i               -- eval({i, I}) -> I
eval (B b) = b               -- eval({b, B}) -> B
eval (S s) = s               -- eval({s, S}) -> S
eval (Add e1 e2) =           -- eval({add, x, y}} ->
  eval e1 + eval e2          --   eval(x) + eval(y)
eval (Mul e1 e2) =
  eval e1 * eval e2
eval (Equal e1 e2) =
  eval e1 == eval e2
eval (Concat e1 e2) =
  eval e1 ++ eval e2

main :: IO ()
main =
  let expr = (Equal (Add (I 5) (I 4)) (I 9)) :: Expr Bool
      x = eval expr
  in putStrLn $ show $ x