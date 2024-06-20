module Parser (
               Parser,
               parser
              ) where

import CoreTypes
import Lexer
import BuiltIns
    
-- ====================================================================================================

parser :: Parser
parser str =
    do (cmds, rest) <- lexer str
       if rest /= ""
       then Left ("Trailing input garbage: '" ++ rest ++ "'")
       else parseCmds cmds

parseCmds :: [Value] -> Result [Value]
parseCmds []                   = return []
parseCmds (ValAtom "[" : cmds) =
    do (sCmds, rest) <- parseStack cmds
       cmds'         <- parseCmds rest
       return $ ValStack sCmds : cmds'
parseCmds (inhibitor@(ValAtom "'") : atom@(ValAtom _) : cmds) =
    do cmds' <- parseCmds cmds
       return $ inhibitor : atom : cmds'
parseCmds (ValAtom atom : cmds) =
    do cmds' <- parseCmds cmds
       return $ (parseAtom atom) : cmds'
parseCmds (cmd : cmds) =
    do cmds' <- parseCmds cmds
       return $ cmd : cmds'

parseAtom :: Name -> Value
parseAtom atom =
    case getBuiltIn atom of
        Nothing -> ValAtom atom
        Just op -> op

parseStack :: [Value] -> Result ([Value], [Value])
parseStack []                   = Left "Missing end of stack marker. (']')"
parseStack (ValAtom "]" : cmds) = return ([], cmds)
parseStack (ValAtom "[" : cmds) = do (sCmds, rest)   <- parseStack cmds
                                     (sCmds', rest') <- parseStack rest
                                     return (ValStack sCmds : sCmds', rest')
parseStack (cmd : cmds)         = do (sCmds, rest) <- parseStack cmds
                                     return (cmd : sCmds, rest)

