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
-- This module contains the interpreter proper of the Stacky
-- language. It deals with operations that are intrinsic to the
-- language. Built-in operations can be found in the BuiltIns module.
--
-------------------------------------------------------------------------------------------------------

module Interpreter (
                    interpreter,
                    runValues,
                    runValue,
                    defApply,
                    runLocalValues
                   ) where

-- Base modules 
import CoreTypes( Cxt(..),     lookupEnv, pushLocal, popLocal
                , Value(..)
                , Result,      ifOk
                , Name
                , Error,       newErrPos, stackUnderflowError)

import Position(Position(..), noPos, isNoPos)

-------------------------------------------------------------------------------------------------------
-- Main interpreter API functions and their helpers
-------------------------------------------------------------------------------------------------------

--
-- Main interpreter entrypoiny
--
interpreter :: Cxt -> [Value] -> IO (Result Cxt)
interpreter = runValues

--
-- Execute a list of operations in a given context.
-- Return the updated context or and error.
--
runValues :: Cxt -> [Value] -> IO (Result Cxt)
runValues cxt []       = return $ Right cxt
runValues cxt (v : vs) = do res <- runValue cxt v
                            ifOk res $ flip runValues vs

--
-- Execute a single operation in a given context.
-- Return the updated context or and error.
--
runValue :: Cxt -> Value -> IO (Result Cxt)
runValue cxt (ValAtom pos atom)   = runAtom pos cxt atom
runValue cxt (ValOp   pos _ op)   = injectPos pos $ op cxt
runValue cxt@Cxt{stack = s} val = leaveVal cxt val s

--
-- Evaluate an atom by looking it up in the environment.
-- If inhibitors are present on the stack, act accordlingly.
--
runAtom :: Position -> Cxt -> Name -> IO (Result Cxt)
runAtom pos cxt@Cxt{stack = s} atom =
    case s of
           (ValAtom _ "'") : s1 -> leaveVal cxt (ValAtom pos atom) s1
           (ValAtom _ "^") : s1 -> lookupAtom cxt atom pos s1  $ \val -> leaveVal cxt val s1
           _                    -> lookupAtom cxt atom noPos s $ \val -> runValue cxt{stack = val : s, callPos = pos} defApply

-------------------------------------------------------------------------------------------------------
-- Auxilliary public functions
-------------------------------------------------------------------------------------------------------

--
-- This is the definition of the apply ('@') operation. It is located
-- here to avoid circular dependencies between the interpreter and the
-- BuiltIns module.
--
defApply :: Value
defApply =
    ValOp noPos "@" $ \cxt@Cxt{stack = s0} ->
           case s0 of
              ValList _ cmds : s1 -> runLocalValues cxt{stack = s1} cmds
              ValAtom p atom : s1 -> runAtom p cxt{stack = s1} atom
              ValOp pos _ op : s1 -> injectPos pos $ op cxt{stack = s1}
              _              : _  -> return $ Right cxt
              _                   -> return $ stackUnderflowError ValNoop "@"

--
-- Evaluate a list of operations in its own local environment, which
-- is discarded upon return. This is howe get local variables.
--
runLocalValues :: Cxt -> [Value] -> IO (Result Cxt)
runLocalValues cxt vs = do res <- runValues (pushLocal cxt) vs
                           ifOk res $ \cxt1 ->
                               return $ Right $ popLocal cxt1

-------------------------------------------------------------------------------------------------------
-- Local helper functions
-------------------------------------------------------------------------------------------------------

--
-- This leaves a new value in a (possibly new stack). Essentially a
-- macro for the incantations needed to return a succesful
-- IO (Result Cxt)
--
leaveVal :: Cxt -> Value -> [Value] -> IO (Either a Cxt)
leaveVal cxt val newStack =
    return $ Right cxt{stack = val : newStack}

--
-- Look up the value of an atom in the environment and continue
-- processing it in `cont`.
--
lookupAtom :: Cxt ->
              Name ->
              Position ->
              [Value] ->
              (Value -> IO (Either Error Cxt)) -> IO (Either Error Cxt)
lookupAtom cxt atom pos s cont = 
    case lookupEnv cxt atom of
        Nothing  | atom `elem` ["'", "^"] -> leaveVal cxt (ValAtom pos atom) s
                 | otherwise           -> return $ newErrPos pos ("Undefined name: '" ++ atom ++ "'")
        Just val -> cont val
                            
--
-- Insert a position into an error message, if none is present.
--
injectPos :: Position -> IO (Result a) -> IO (Result a)
injectPos pos iop =
    iop >>= \res ->
               return $ case res of
                            l@(Left (p, err)) | isNoPos p  -> Left (pos, err)
                                              | otherwise  -> l
                            right                          -> right

-------------------------------------------------------------------------------------------------------
--  That's all folks!!
-------------------------------------------------------------------------------------------------------

