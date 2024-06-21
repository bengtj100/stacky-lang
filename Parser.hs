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
parseCmds bIns (ValAtom "[" : cmds) =
    do (sCmds, rest) <- parseStack bIns cmds
       cmds'         <- parseCmds bIns rest
       return $ ValStack sCmds : cmds'
parseCmds bIns (inhibitor@(ValAtom "'") : atom@(ValAtom _) : cmds) =
    do cmds' <- parseCmds bIns cmds
       return $ inhibitor : atom : cmds'
parseCmds bIns (cmd : cmds) =
    do cmd'  <- parseCmd bIns cmd
       cmds' <- parseCmds bIns cmds
       return $ cmd' : cmds'

parseCmd :: Env -> Value -> Result Value
parseCmd bIns (ValAtom atom) =
    return $ case lookup atom bIns of
                 Nothing -> ValAtom atom
                 Just op -> op
parseCmd _ cmd =
    return cmd

parseStack :: Env -> [Value] -> Result ([Value], [Value])
parseStack _ [] =
    Left "Missing end of stack marker. (']')"
parseStack _ (ValAtom "]" : cmds) =
    return ([], cmds)
parseStack bIns (ValAtom "[" : cmds) =
    do (sCmds, rest)   <- parseStack bIns cmds
       (sCmds', rest') <- parseStack bIns rest
       return (ValStack sCmds : sCmds', rest')
parseStack bIns (cmd : cmds) =
    do cmd'          <- parseCmd bIns cmd
       (cmds', rest) <- parseStack bIns cmds
       return (cmd' : cmds', rest)

-- ====================================================================================================
