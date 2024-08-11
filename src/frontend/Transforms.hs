-------------------------------------------------------------------------------------------------------
--
-- Copyright (c) 2024 Bengt Johansson <bengtj100 at gmail dot com>.
-- All rights reserved.
--
-- This software is part of the stacky project and its use is
-- regulated by the conditions stipulated in the file named 'LICENCE',
-- located in the top directory of said project.
--
-------------------------------------------------------------------------------------------------------
--
-- This module performs transformations and optimizations on the
-- output of the parser befor handing over to the backend.
--
-------------------------------------------------------------------------------------------------------

module Transforms (
               transform
              ) where

-- Base modules
import CoreTypes
    
-------------------------------------------------------------------------------------------------------
--  Main transform API
-------------------------------------------------------------------------------------------------------

--
-- Perform all source code transformations:
--   1) Remove any Noops from the code
--   2) Change atoms referencing built-ins to the actual operation.
--      (Except when affected by an inhibitor
--
transform :: Env -> [Value] -> [Value]
transform = tCmds

--
-- Perform transformations on lists of commands
--
tCmds :: Env -> [Value] -> [Value]
tCmds bi (ValNoop : cs)                                  = tCmds bi cs
tCmds bi (inh@(ValAtom _ "'") : atom@(ValAtom _ _) : cs) = inh : atom : tCmds bi cs
tCmds bi (inh@(ValAtom _ "^") : atom@(ValAtom _ _) : cs) = inh : atom : tCmds bi cs
tCmds bi (ValList p l : cs)                              = ValList p (tCmds bi l) : tCmds bi cs
tCmds bi (c : cs)                                        = tCmd bi c : tCmds bi cs
tCmds _  []                                              = []
                                                         
--
-- Perform transformations on individual commands
--
tCmd :: Env -> Value -> Value
tCmd bi (ValAtom p atom) = case lookup atom bi of
                               Nothing                -> ValAtom p atom
                               Just (ValOp _ name op) -> ValOp p name op
                               Just op                -> intError op
tCmd _ cmd               = cmd

--
-- Helper function for internal errors
--
intError :: Value -> a
intError op = error $ "INTERNAL ERROR! Built-in operation is not a ValOp: '" ++ show op ++ "'"
