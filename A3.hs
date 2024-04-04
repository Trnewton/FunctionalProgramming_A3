module A3 where

{- This module is for playing with functions in ghci..
 - In other words, type
 -  ``ghci A3.hs``
 - so you can play with your code from the modules imported below..
 -}


import Lib.AST
import Lib.ASTParse
import CES
import DeBruijn
import Examples

-- Sum, given N return sum_{n=1}^N N
test1 = App
        (FFix1
                ("f", "N")
                (BIf
                        (BLeq (Var "N") (Const 0))
                        (Const 0)
                        (Add
                                (Var "N")
                                (App
                                        (Var "f")
                                        (Add
                                                (Var "N")
                                                (Const (-1))
                                        )
                                )
                        )
                )
        )
        (Const 6)
d1 = case autoGraderToDeBruijn test1 of
        Left err    -> error err
        Right t     -> t
ces1 = compileToCes d1

-- Factorial
test2 = App (FFix1 ("f", "x") (BIf (BLeq (Var "x") (Const 0)) (Const 1) (Mul (Var "x") (App (Var "f") (Add (Var "x") (Const (-1))))))) (Const 6)
d2 = case autoGraderToDeBruijn test2 of
        Left err    -> error err
        Right t     -> t
ces2 = compileToCes d2

-- Fancy sum, takes an N and a g and computes $\sum_{n=1}^{N} g(n)$
test3 = App
                (App
                        (Abs
                                ("g", [])
                                (FFix1
                                        ("f", "x")
                                        (BIf
                                                (BLeq (Var "x") (Const 0))
                                                (Const 0)
                                                (Add
                                                        (App
                                                                (Var "g")
                                                                (Var "x")
                                                        )
                                                        (App
                                                                (Var "f")
                                                                (Add
                                                                        (Var "x")
                                                                        (Const (-1))
                                                                )
                                                        )
                                                )
                                        )
                                )
                        )
                        -- Example g: g(n) = 3n^2
                        (Abs
                                ("y", [])
                                (Mul (Mul (Const 3) (Var "y")) (Var "y"))
                        )
                )
        -- Example N
        (Const 7)
d3 = case autoGraderToDeBruijn test3 of
        Left err    -> error err
        Right t     -> t
ces3 = compileToCes d3

-- For loop, takes g(x), x_0, and N and applies g(g(...g(x_0))) N times
test4 = App
                (App
                        (App
                                (Abs
                                        ("g", ["x"])
                                        (FFix1
                                                ("f", "n")
                                                (BIf
                                                        (BLeq (Var "n") (Const 0))
                                                        (Var "x")
                                                        (App
                                                                (Var "g")
                                                                (App
                                                                        (Var "f")
                                                                        (Add
                                                                                (Var "n")
                                                                                (Const (-1))
                                                                        )
                                                                )
                                                        )
                                                )
                                        )
                                )
                                -- Example g: g(x) = x+1
                                (Abs
                                        ("y", [])
                                        (Add (Var "y") (Const 1))
                                )
                        )
                        -- Initial x_0
                        (Const 1)
                )
        -- Example N
        (Const 7)
d4 = case autoGraderToDeBruijn test4 of
        Left err    -> error err
        Right t     -> t
ces4 = compileToCes d4

-- Subtraction, x-y
test5 = App
                (App
                        (Abs
                                ("x",[])
                                (FFix1
                                        ("loop", "y")
                                        (BIf
                                                (BLeq (Var "y") (Const 0))
                                                (Var "x")
                                                (Add
                                                        (Const (-1))
                                                        (App
                                                                (Var "loop")
                                                                (Add
                                                                        (Const (-1))
                                                                        (Var "y")
                                                                )
                                                        )
                                                )
                                        )
                                )
                        )
                        -- x
                        (Const 15)
                )
        -- y
        (Const 6)
d5 = case autoGraderToDeBruijn test5 of
        Left err    -> error err
        Right t     -> t
ces5 = compileToCes d5

test6 = unsafeParseLambdaTerm myMap
d6 = case autoGraderToDeBruijn test6 of
        Left err    -> error err
        Right t     -> t
ces6 = compileToCes d6

test7 = unsafeParseLambdaTerm factorial
d7 = case autoGraderToDeBruijn test7 of
        Left err    -> error err
        Right t     -> t
ces7 = compileToCes d7


test8 = unsafeParseLambdaTerm fancySum
d8 = case autoGraderToDeBruijn test8 of
        Left err    -> error err
        Right t     -> t
ces8 = compileToCes d8


test9 = unsafeParseLambdaTerm forLoop
d9 = case autoGraderToDeBruijn test9 of
        Left err    -> error err
        Right t     -> t
ces9 = compileToCes d9


test10 = unsafeParseLambdaTerm power
d10 = case autoGraderToDeBruijn test10 of
        Left err    -> error err
        Right t     -> t
ces10 = compileToCes d10

test11 = unsafeParseLambdaTerm f
d11 = case autoGraderToDeBruijn test11 of
        Left err    -> error err
        Right t     -> t
ces11 = compileToCes d11