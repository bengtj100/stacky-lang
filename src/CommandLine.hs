
module CommandLine(
                   CmdArgs(..),
                   parseArguments
                   ) where

import CoreTypes
    
-- ====================================================================================================

data CmdArgs = CmdArg { interactive :: Bool,
                        prelude     :: [Value],
                        preludeFile :: String }
             | CmdError String
             | CmdUsage
             | CmdVersion

newCmdArgs :: CmdArgs
newCmdArgs = CmdArg{interactive = True, prelude = [], preludeFile = ""}

-- ====================================================================================================

parseArguments :: [String] -> CmdArgs
parseArguments args = parseArgs args newCmdArgs

parseArgs :: [String] -> CmdArgs -> CmdArgs
parseArgs (opt : code : args) cargs
    | opt `elem` ["--eval", "-e"]        = parseArgs args $ makeEval cargs code
    | opt `elem` ["--prelude"]           = parseArgs args $ makePrelude cargs code
parseArgs [opt] _
    | opt `elem` ["--eval", "-e"]        = tooFewArgsError opt 1 0
parseArgs (opt : args) cargs
    | opt `elem` ["--interactive", "-i"] = parseArgs args $ makeInteractive cargs True
    | opt `elem` ["--batch",       "-b"] = parseArgs args $ makeInteractive cargs False
    | opt `elem` ["--version"          ] = CmdVersion
    | opt `elem` ["--help",        "-h"] = CmdUsage
parseArgs (err@('-':_) : _   ) _          = unknownOptError err
parseArgs (fName       : args) cargs      = parseArgs args $ makeImport cargs fName
parseArgs []                   cargs      = cargs
                                           
-- ====================================================================================================

makeEval :: CmdArgs -> String -> CmdArgs
makeEval ca@CmdArg{prelude = p} code =
    ca{prelude = p ++ [ValString noPos code, ValAtom noPos "eval"]}
makeEval _ _ =
    error "INTERNAL ERROR in CommandLine.makeEval"

makePrelude :: CmdArgs -> String -> CmdArgs
makePrelude ca@CmdArg{} fName =
    ca{preludeFile = fName}
makePrelude _ _ =
    error "INTERNAL ERROR in CommandLine.makePrelude"

makeImport :: CmdArgs -> String -> CmdArgs
makeImport ca@CmdArg{prelude = p} fName
    = ca{prelude = p ++ [ValString noPos fName, ValAtom noPos "import"]}
makeImport _ _ =
    error "INTERNAL ERROR in CommandLine.makeImport"
    
makeInteractive :: CmdArgs -> Bool -> CmdArgs
makeInteractive ca@CmdArg{} i = ca{interactive = i}
makeInteractive _           _ = error "INTERNAL ERROR in CommandLine.makeInteractive"

-- ====================================================================================================

unknownOptError :: String -> CmdArgs
unknownOptError str =
    CmdError ("Undefined option: '" ++ str ++ "'")

tooFewArgsError :: String -> Int -> Int -> CmdArgs
tooFewArgsError opt expArgs actArgs =
    CmdError ("Too few arguments for '" ++ opt ++ "'! "
              ++ "Expected " ++ show expArgs ++ ", got " ++ show actArgs ++ ".")
