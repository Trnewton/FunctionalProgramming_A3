module DeBruijn where

{- This module is for functions relating to
 - converting a LambdaTerm to its DeBruijnTerm..
 -}

import Lib.AST
import Lib.Monads

-- | ``autoGraderToDebruijn``
-- This function will be used by the autograder to test if your
-- code is correct!
-- Please write this function so that it converts a LambdaTerm
-- to its corresponding DeBruijnTerm..
--
--  - You MUST start indexing at 0.
--
-- For example, the lambda term ``\x . x`` should be ``\ _ . #0``
-- where ``#0`` denotes the DeBruijn index and _ denotes the removed
-- argument (note how from class we would normally just write
-- ``\ . #0`` for the DeBruijn notation).
--
-- Moreover, this function should ``desugar`` multiple arguments
-- ONLY for lambda abstractions...
-- For example, the lambda term ``\a b c -> a`` should be changed
-- to ``\a -> \b -> \c -> a``.
--
-- In the case that there is a free variable, please return:
--      ``Left "some helpful error message here"``.
autoGraderToDeBruijn :: LambdaTerm -> Either String DeBruijnTerm
autoGraderToDeBruijn t = evalStateT (lambToDeBruijn t) []

-- | Stack for storing variable names in DeBruijn renaming
type DBStack = [String]

-- | Looks up a value on a stack and if found returns its depth
lookUp :: (Eq a) => [a] -> a -> Maybe Int
lookUp as a = lookUp' as a 0
    where
        lookUp' :: (Eq a) => [a] -> a -> Int -> Maybe Int
        lookUp' [] _ _ = Nothing
        lookUp' (a:as) b n
            | a == b    = Just n
            | otherwise = lookUp' as b (n+1)

-- | Pushes multiple values onto a stack and returns the stack
pushMult :: [a] -> [a] -> [a]
pushMult vs s = reverse vs ++ s

-- | Pops a given number of values from a stack
popMult :: Int -> [a] -> [a]
popMult n [] = []
popMult n at@(a:as)
    | n==0      = at
    | otherwise = popMult (n-1) as

-- | Renames a lambda expression into DeBruijn notation
lambToDeBruijn :: LambdaTerm -> StateT DBStack (Either String) DeBruijnTerm
-- Abs, App, Var
lambToDeBruijn (Abs (v, vs) t) = do
    modify (v:)
    modify (pushMult vs)
    t' <- lambToDeBruijn t
    modify (popMult (length vs))
    modify tail
    return (Abs () (foldr (\p t'' -> Abs () t'') t' vs))
lambToDeBruijn (App t1 t2) = do
    t1' <- lambToDeBruijn t1
    t2' <- lambToDeBruijn t2
    return (App t1' t2')
lambToDeBruijn (Var var) =
    StateT (\s -> case lookUp s var of -- lookup and if fail then error
            Nothing     -> Left ("Free variable found." ++ show var)
            (Just v)    -> Right (Var v,s))
-- Const, add, mul, leq
lambToDeBruijn (Const n) = do return (Const n)
lambToDeBruijn (Add t1 t2) = do
    t1' <- lambToDeBruijn t1
    t2' <- lambToDeBruijn t2
    return (Add t1' t2')
lambToDeBruijn (Mul t1 t2) = do
    t1' <- lambToDeBruijn t1
    t2' <- lambToDeBruijn t2
    return (Mul t1' t2')
lambToDeBruijn (BLeq t1 t2) = do
    t1' <- lambToDeBruijn t1
    t2' <- lambToDeBruijn t2
    return (BLeq t1' t2')
-- Boolean operations
lambToDeBruijn BTrue = do return BTrue
lambToDeBruijn BFalse = do return BFalse
lambToDeBruijn (BIf t1 t2 t3) = do
    t1' <- lambToDeBruijn t1
    t2' <- lambToDeBruijn t2
    t3' <- lambToDeBruijn t3
    return (BIf t1' t2' t3')
-- Nils, cons and cases
lambToDeBruijn LNil = do return LNil
lambToDeBruijn (LCons t1 t2) = do
    t1' <- lambToDeBruijn t1
    t2' <- lambToDeBruijn t2
    return (LCons t1' t2')
lambToDeBruijn (LCase t1 t2 ((sb1,sb2),t3)) = do
    t1' <- lambToDeBruijn t1
    t2' <- lambToDeBruijn t2
    modify (sb1:)
    modify (sb2:)
    t3' <- lambToDeBruijn t3
    modify (tail.tail)
    return (LCase t1' t2' (((),()),t3'))
-- Fixed point
lambToDeBruijn (FFix0 sb t) = do
    modify (sb:)
    t' <- lambToDeBruijn t
    modify tail
    return (FFix0 () t')
lambToDeBruijn (FFix1 (sb1, sb2) t) = do
    modify (sb1:)
    modify (sb2:)
    t' <- lambToDeBruijn t
    modify (tail.tail)
    return (FFix1 ((),()) t')