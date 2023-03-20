{-# LANGUAGE QuasiQuotes #-}
module Examples where

import Lib.RawString
import Lib.AST

{- Here, you can write some example programs as Raw String literals to 
 - test your code..
 -}

{- Some remarks about the parser..
 -
 - To use the parser (see Lib.ASTParse for more details), write 
 -      ``unsafeParseLambdaTerm <some lambda term here>``
 - and this should return a LambdaTerm, but otherwise, it will throw an (uninformative) error..
 -
 -
 -  This parser will either accept:
 -      ``\x . x`` 
 -     or 
 -      ``\x -> x`` 
 -  as a lambda abstraction.
 -
 -  Infixity of binary operations copies Haskell, i.e., the infixity is defined as follows:
 -  ```
    infix 4 <=
    infixr 5 :
    infixl 6 +
    infixl 7 *
    ```

   So, for example, writing:
    ``\x -> f x + x * 3``
   will be parsed as
    ``\x -> (f x) + (x * 3)``
 -
 - You may write ``--`` for line comments (but no multiline comments are supported!).
 - So, for example, you can write
 -  ```
    -- (\x -> x)
    \f -> (\x . f x x ) (\x . f x x )
 -  ```
 -  and the parser will just parse ``\f -> (\x . f x x ) (\x . f x x )``.
 - 
 - The list case construct (written as ``lcase``) REQUIRES both cases to be written, AND
 - a semicolon to separate the two cases.
 - For example, to take the head of the list ``[1,2,3]``, write:
 -  ``(\inp -> lcase inp of 
 -      [] -> 0 ;  -- << NOTE THE SEMICOLON WRITTEN HERE IS VERY IMPORTANT
 -      a:as -> a) [1,2,3]
 -  ``
 - But, writing 
 -  ``(\inp -> lcase inp of 
 -              [] -> 0  
 -              a:as -> a) 
 -    [1,2,3]
 -  ``
 - would be a parse error since it is missing the semicolon between the two cases..
 -
 - Parsing the built-in ``fix`` function (for recursion) is a little strange...
 - We write some examples to illustrate.
 - If we write:
 -  ``fix (\f -> 3)`` 
 - then, this parses as 
 -  ``FFix0 "f" (Const 3)``
 -
 - If we write:
 -  ``fix (\f a -> 3)`` 
 - then, this parses as 
 -  ``FFix1 ("f","a") (Const 3)``
 - (note the change of constructors)
 -
 - But if we write:
 -  ``fix (\f a b -> 3)``
 - then, this parses as  
 -  ``FFix1 ("f","a") (Abs ("b",[]) (Const 3))``
 - (note how it automatically ``flattens`` the extra abstractions, but this only happens with the built in fix function).
 - This is done to save you some headaches later with how the machine handles fixed points! 
 -
 - Below, we include some example programs (but you should write your own too!)
 - to get a better feel for the grammar...
 -}

--------------------
 -- Write your own examples:
--------------------


--------------------
-- Given examples:
--------------------
assign0 :: String
assign0 = [r|
    (\x -> x + 1)  3 
|]

higherordersucc :: String
higherordersucc = [r|
    (\ a -> a 3 ) (\x -> x + 1)
|]

zipwithsum :: String
zipwithsum = [r|
    fix (\f a b ->  
        lcase a of
            [] -> [] ;
            a:as -> lcase b of
                [] -> [] ;
                b : bs -> a + b : f as bs
        )
        [1,2]
        [3,4]
|]

churchNumeralThreeToInt :: String
churchNumeralThreeToInt = [r|
    (\z s -> s (s (s z))) 0 (\s -> s + 1)
|]

churchListSum :: String
churchListSum = [r|
    (\c n -> c 3 (c 2 (c 1(n)))) (\a \acc -> a + acc) 0
|]

headoflist :: String
headoflist = [r|
    (\inp -> lcase inp of [] -> 0 ; a:as -> a) [1,2,3]
|]

headofemptylist :: String
headofemptylist = [r|
    (\inp -> lcase inp of [] -> 0 ; a:as -> a) ((\a -> a) [])
|]

sumoflist :: String
sumoflist = [r|
    fix 
        (\loop inp -> lcase inp of 
            [] -> 0 ; 
            a:as -> a + loop as
        ) 
        [1,2,3,4]
|]

computingfactorial :: String
computingfactorial = [r|
    fix (\loop inp -> if inp <= 1 then 1 else inp * loop (inp + (-1) ) ) 10
|]
