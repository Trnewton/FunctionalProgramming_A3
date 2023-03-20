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
autoGraderToDeBruijn :: LambdaTerm -> 
    Either String DeBruijnTerm
autoGraderToDeBruijn = undefined
