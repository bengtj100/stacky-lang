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

{-# LANGUAGE QuasiQuotes #-}

module CommandLine(
                   CmdRes(..),
                   parseArguments,
                   printGreeting
                   ) where

import Text.RawString.QQ
import System.Exit
import System.Environment

import Version
import CoreTypes
    
-- ====================================================================================================

data CmdRes = CmdRes{ interactive :: Bool,
                      prelude     :: [Value],
                      preludeFile :: String }
            
data CmdArgs = CmdArg CmdRes
             | CmdError String
             | CmdUsage
             | CmdVersion

newCmdArgs :: CmdArgs
newCmdArgs = CmdArg CmdRes{interactive = True, prelude = [], preludeFile = ""}

-- ====================================================================================================

parseArguments :: [String] -> IO CmdRes
parseArguments args =
    case carg of
        CmdError errMsg -> printCmdError errMsg
        CmdVersion      -> printVersion
        CmdUsage        -> printUsage
        CmdArg res      -> return res
    where
        carg = parseArgs args newCmdArgs

-- ----------------------------------------------------------------------------------------------------

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

-- ----------------------------------------------------------------------------------------------------

printUsage :: IO a
printUsage =
    do pName <- getProgName
       putStrLn $ usageStr pName
       exitWith ExitSuccess

-- ----------------------------------------------------------------------------------------------------

usageStr :: String -> String
usageStr pName =
    [r|
Usage: |] ++ pName ++ [r| ( <option> | <module-name> )*

Where option is one of:

--eval, -e <stacky-code>    Evaluate <stacky-code>

--interactive, -i           Run in interactive mode, i.e., run the REPL after Prelude and
                            all modules are loaded.

--batch, -b                 Run in batch mode, i.e., terminate once all modules are loaded.

--prelude <path>            Load the prelude from <path> instead of the default location.

--version                   Print the current version and terminate.

--help, -h                  Print this message and then terminate.

The interpreter will load all modules and execute the '--eval' options in the
order they appear on the command line. If in interactive mode, the REPL will
run after all loading is complete and in batch mode, the interpreter will
terminate once all is loaded and executed.

|]

-- ----------------------------------------------------------------------------------------------------

printCmdError :: String -> IO a
printCmdError errMsg =
    do printErrorWithProgname (noPos, errMsg)
       exitWith (ExitFailure 2)
                
-- ----------------------------------------------------------------------------------------------------

printVersion :: IO a
printVersion =
    do putStrLn $ "## Stacky - Copyright (c) 2024 Bengt Johansson -- All rights reserved"
       putStrLn $ "VERSION=" ++ version
       putStrLn $ "BUILD="   ++ build
       putStrLn $ "GIT_TAG=" ++ gitTag
       exitWith ExitSuccess

printGreeting :: IO ()
printGreeting =
    do putStrLn ""
       putStrLn $ "Stacky, version: " ++ version ++", build: " ++ build
       putStrLn $ "Copyright (c) 2024 Bengt Johansson -- All rights reserved"
       putStrLn ""

-- ====================================================================================================

makeEval :: CmdArgs -> String -> CmdArgs
makeEval (CmdArg res@(CmdRes{prelude = p})) code =
    CmdArg res{prelude = p ++ [ValString noPos code, ValAtom noPos "eval"]}
makeEval _ _ =
    error "INTERNAL ERROR in CommandLine.makeEval"

makePrelude :: CmdArgs -> String -> CmdArgs
makePrelude (CmdArg res) fName =
    CmdArg res{preludeFile = fName}
makePrelude _ _ =
    error "INTERNAL ERROR in CommandLine.makePrelude"

makeImport :: CmdArgs -> String -> CmdArgs
makeImport (CmdArg (res@CmdRes{prelude = p})) fName
    = CmdArg res{prelude = p ++ [ValString noPos fName, ValAtom noPos "import"]}
makeImport _ _ =
    error "INTERNAL ERROR in CommandLine.makeImport"
    
makeInteractive :: CmdArgs -> Bool -> CmdArgs
makeInteractive (CmdArg res) i =
    CmdArg res{interactive = i}
makeInteractive _ _ =
    error "INTERNAL ERROR in CommandLine.makeInteractive"

-- ====================================================================================================

unknownOptError :: String -> CmdArgs
unknownOptError str =
    CmdError ("Undefined option: '" ++ str ++ "'")

tooFewArgsError :: String -> Int -> Int -> CmdArgs
tooFewArgsError opt expArgs actArgs =
    CmdError ("Too few arguments for '" ++ opt ++ "'! "
              ++ "Expected " ++ show expArgs ++ ", got " ++ show actArgs ++ ".")
