{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Calc where
import ExprT
import Parser
import qualified StackVM

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
    Just exp -> Just $ eval exp
    Nothing -> Nothing

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit n
        | n <= 0 = False
        | otherwise = True
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax m) (MinMax n) = lit $ max m n
    mul (MinMax m) (MinMax n) = lit $ min m n

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit n = Mod7 $ mod n 7
    add (Mod7 m) (Mod7 n) = lit (m + n)
    mul (Mod7 m) (Mod7 n) = lit (m * n)

instance Expr StackVM.Program where
    lit n = [StackVM.PushI n]
    add p1 p2 = p2 ++ p1 ++ [StackVM.Add]
    mul p1 p2 = p2 ++ p1 ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile source = parseExp lit add mul source :: Maybe StackVM.Program