-- ====================================================================================================
--
-- Copyright (c) 2024 Bengt Johansson <bengtj100 at gmail dot com>.
-- All rights reserved.
--
-- This software is part of the stacky project and its use is
-- regulated by the conditions stipulated in the file named 'LICENCE',
-- located in the top directory of said project.
--
-- ====================================================================================================

module Parser (
               Parser,
               parser
              ) where

import CoreTypes
    
-- ====================================================================================================

parser :: Env -> Parser
parser = parseCmds

parseCmds :: Env -> [Value] -> Result [Value]
parseCmds _ [] =
    return []
parseCmds bIns (ValNoop : cmds) =
    parseCmds bIns cmds
parseCmds bIns (ValAtom pos "[" : cmds) =
    do (sCmds, rest) <- parseList bIns cmds
       cmds'         <- parseCmds bIns rest
       return $ ValList pos sCmds : cmds'
parseCmds bIns (inhibitor@(ValAtom _ "'") : atom@(ValAtom _ _) : cmds) =
    do cmds' <- parseCmds bIns cmds
       return $ inhibitor : atom : cmds'
parseCmds bIns (inhibitor@(ValAtom _ "^") : atom@(ValAtom _ _) : cmds) =
    do cmds' <- parseCmds bIns cmds
       return $ inhibitor : atom : cmds'
parseCmds bIns (cmd : cmds) =
    do cmd'  <- parseCmd bIns cmd
       cmds' <- parseCmds bIns cmds
       return $ cmd' : cmds'

parseCmd :: Env -> Value -> Result Value
parseCmd bIns (ValAtom pos atom) =
    case lookup atom bIns of
        Nothing ->
            Right $ ValAtom pos atom
        Just (ValOp _ name op) ->
            Right $ ValOp pos name op
        Just op ->
            newError op $ "INTERNAL ERROR! A BUILTIN FUNCTION THAT IS NOT A ValOp: '" ++ show op ++ "'"
parseCmd _ cmd =
    return cmd

parseList :: Env -> [Value] -> Result ([Value], [Value])
parseList _ [] =
    newErrPos noPos "Missing end of list marker. (']')"
parseList _ (ValAtom _ "]" : cmds) =
    return ([], cmds)
parseList bIns (ValAtom pos "[" : cmds) =
    do (sCmds, rest)   <- parseList bIns cmds
       (sCmds', rest') <- parseList bIns rest
       return (ValList pos sCmds : sCmds', rest')
parseList bIns (cmd : cmds) =
    do cmd'          <- parseCmd bIns cmd
       (cmds', rest) <- parseList bIns cmds
       return (cmd' : cmds', rest)

-- ====================================================================================================
