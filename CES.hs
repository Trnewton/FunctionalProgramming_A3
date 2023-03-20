module CES where

{- This module is for functions relating to 
 - the CES machine including:
 -      - compiling a DeBruijn notation expression
 -      - doing a step on the CES machine
 -      - running a CES machine
 -}

import Lib.AST

-- | ``autoGraderCompileToCes``
-- This function will be used by the autograder to test if your 
-- code is correct!
-- Please write this function so that it converts a DeBruijnTerm to 
-- a list of CES instructions...
--
-- See pg 16  of ``ces.pdf``
autoGraderCompileToCes :: DeBruijnTerm -> [Instr]
autoGraderCompileToCes = undefined


-- | ``autoGraderRunCes``
-- This function will be used by the autograder to test if your 
-- code is correct!
-- Please write this function so that it will run a CES machine
-- until the termination condition is reached (if it exists!)
--
-- See pg. 17 of ``ces.pdf``
autoGraderRunCes :: CES -> Instr
autoGraderRunCes (code, env, stack) = undefined
