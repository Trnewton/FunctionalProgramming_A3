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
autoGraderCompileToCes = compileToCes

-- | Compiles DeBruin term to CES instruction
compileToCes :: DeBruijnTerm -> [Instr]
-- Abstractions, applications, variables
compileToCes (Abs () t) = [IClo (compileToCes t ++ [IRet],[])]
compileToCes (App m n) = compileToCes n  ++ compileToCes m ++ [IApp]
compileToCes (Var x) = [IAccess x]
-- Constants, arithmatic operations
compileToCes (Const k) = [IConst k]
compileToCes (Add a b) = compileToCes b ++ compileToCes a ++ [IAdd]
compileToCes (Mul a b) = compileToCes b ++ compileToCes a ++ [IMul]
compileToCes (BLeq a b) = compileToCes b ++ compileToCes a ++ [ILeq]
-- Boolean logic
compileToCes (BIf t t0 t1) = (compileToCes t) ++ [IIf (compileToCes t0 ++ [IRet],
                                                        compileToCes t1 ++ [IRet])]
compileToCes BTrue = [ITrue]
compileToCes BFalse = [IFalse]
-- Nils, cons, and case
compileToCes LNil = [INil]
compileToCes (LCons a b) = compileToCes b ++ compileToCes a ++ [ICons Nothing]
compileToCes (LCase t t0 (((),()),t1)) = compileToCes t ++ [ICase (compileToCes t0 ++ [IRet],
                                                        compileToCes t1 ++ [IRet])]
-- Fixed points
compileToCes (FFix0 () n) = [IFFix0 (compileToCes n ++ [IRet], [])]
compileToCes (FFix1 ((),()) n) = [IFFix1 (compileToCes n ++ [IRet], [])]



-- | ``autoGraderRunCes``
-- This function will be used by the autograder to test if your
-- code is correct!
-- Please write this function so that it will run a CES machine
-- until the termination condition is reached (if it exists!)
--
-- See pg. 17 of ``ces.pdf``
autoGraderRunCes :: CES -> Instr
autoGraderRunCes = runCes

-- | Takes a CES instance and runs it until termination
runCes :: CES -> Instr
-- Computation complete
runCes input@([], _, ans:_) = ans
-- Clo, App, Access, Ret
runCes input@((IClo (c', e')):c, e, s) = runCes (c, e, (IClo (c', e)):s)
runCes input@(IApp:c, e, (IClo (c', e')):v:s) = runCes (c', v:e', (IClo (c,e)):s)
runCes input@((IAccess n):c, e, s) = runCes (c, e, (e!!n):s)
runCes input@(IRet:c, e, v:(IClo (c',e')):s) = runCes (c', e', v:s)
-- Const, add, mul, leq
runCes input@((IConst k):c, e, s) = runCes (c, e, (IConst k):s)
runCes input@(IAdd:c, e, (IConst n):(IConst m):s) = runCes (c, e, (IConst (n+m)):s)
runCes input@(IMul:c, e, (IConst n):(IConst m):s) = runCes (c, e, (IConst (n*m)):s)
runCes input@(ILeq:c, e, (IConst n):(IConst m):s) = runCes (c, e, (case n <= m of
                        True    -> ITrue
                        False   -> IFalse
                    ):s)
-- Boolean operations
runCes input@(ITrue:c, e, s) = runCes (c, e, ITrue:s)
runCes input@(IFalse:c, e, s) = runCes (c, e, IFalse:s)
runCes input@((IIf (c0, c1)):c, e, ITrue:s) = runCes (c0, e, (IClo (c,e)):s)
runCes input@((IIf (c0, c1)):c, e, IFalse:s) = runCes (c1, e, (IClo (c,e)):s)
-- Nils, cons and cases
runCes input@(INil:c, e, s) = runCes (c, e, INil:s)
runCes input@((ICons (Nothing)):c, e, v1:v2:s) = runCes (c, e, (ICons (Just (v1, v2))):s)
runCes input@((ICase (c1,c2)):c, e, (ICons (Just (v1,v2))):s) = runCes (c2, v2:v1:e, (IClo (c,e)):s)
runCes input@((ICase (c1,c2)):c, e, INil:s) = runCes (c1, e, (IClo (c,e)):s)
-- Fixed point
runCes input@((IFFix1 (c',e')):c, e, s) = runCes (c,e,(IFFix1 (c',e)):s)
runCes input@(IApp:c, e, (IFFix1 (c',e')):v:s) = runCes (c', v:(IFFix1 (c',e')):e', (IClo (c,e)):s)
runCes input@((IFFix0 (c',e')):c, e, s) = runCes (c,e,(IFFix0 (c',e)):s)
runCes input@(IApp:c, e, (IFFix0 (c',e')):s) = runCes (c', (IFFix0 (c',e')):e', (IClo (c,e)):s)