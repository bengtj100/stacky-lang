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
-- This module parses and collect command-line arguments to the
-- interpreter.
--
-------------------------------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes #-}

module CommandLine(
                   CmdRes(..),
                   parseArguments
                   ) where

-- System modules
import Text.RawString.QQ(r)
import System.Exit(exitWith, ExitCode(..))
import System.Environment(getProgName)

-- Base modules
import CoreTypes(Value(..), printErrorWithProgname)
import Position(noPos)

-- Local modules
import Version(build, gitTag, version)

-------------------------------------------------------------------------------------------------------
--  Main API
-------------------------------------------------------------------------------------------------------

--
-- This is the type that collects information from command-line that
-- needs to be passed on to the rest of the program.
--

data CmdRes = CmdRes{ interactive :: Bool      -- True = Interactive mode. Run the REPL
                    , prelude     :: [Value]   -- Operations to run as the prelude.
                    , preludeFile :: String    -- Name of the Prelude file.
                    , incPrePaths :: [String]  -- List of paths to prepend to STACKY_INCLUDE_PATH
                    , incAppPaths :: [String]  -- List of paths to append to STACKY_INCLUDE_PATH
                    }
-------------------------------------------------------------------------------------------------------

--
-- Parse command-line arguments and either collect them in a CmdRes or
-- act on high precedence option or error.
--
parseArguments :: [String] -> IO CmdRes
parseArguments args =
    case carg of
        CmdError errMsg -> printCmdError errMsg
        CmdVersion      -> printVersion
        CmdUsage        -> printUsage
        CmdArg res      -> return res
    where
        carg = parseArgs args newCmdArgs

-------------------------------------------------------------------------------------------------------
--  Internal command-line parsing functions
-------------------------------------------------------------------------------------------------------

--
-- This is the result of parsing the command-line options
--
data CmdArgs = CmdArg CmdRes    -- Collected options for the interpreter
             | CmdError String  -- Something went wrong. There is an error message.
             | CmdUsage         -- The usage message was requested.
             | CmdVersion       -- The version message was requested.

-------------------------------------------------------------------------------------------------------

--
-- Initialize a normal instance of CmdArgs
--
newCmdArgs :: CmdArgs
newCmdArgs = CmdArg CmdRes{ interactive = True
                          , prelude     = []
                          , preludeFile = ""
                          , incPrePaths = []
                          , incAppPaths = []
                          }

makeCA :: (CmdRes -> CmdRes) -> CmdArgs -> CmdArgs
makeCA f (CmdArg res) = CmdArg $ f res
makeCA _ _            = error "INTERNAL ERROR in CommandLine.makeCmdArgs"

--
-- Add commands to the prelude string to eval a string of code.
--
makeEval :: String -> CmdArgs -> CmdArgs
makeEval code = makeCA $ \res@CmdRes{prelude = p} ->
                   res{prelude = p ++ [ValString noPos code, ValAtom noPos "eval"]}

--
-- Add commands to the prelude string to import a module.
--
makeImport :: String -> CmdArgs -> CmdArgs
makeImport fName = makeCA $ \res@CmdRes{prelude = p} ->
                       res{prelude = p ++ [ValString noPos fName, ValAtom noPos "import"]}

--
-- Set the name of the prelude file to load the prelude from.
--
makePrelude :: String -> CmdArgs -> CmdArgs
makePrelude fName = makeCA $ \res -> res{preludeFile = fName}

--
-- Prepend path to STACKY_INCLUDE_PATH
--
makeIncPrePaths :: String -> CmdArgs -> CmdArgs
makeIncPrePaths pre = makeCA $ \res@CmdRes{incPrePaths = pres} -> res{incPrePaths = pre : pres}

--
-- Append path to STACKY_INCLUDE_PATH
--
makeIncAppPaths ::  String -> CmdArgs ->CmdArgs
makeIncAppPaths app = makeCA $ \res@CmdRes{incAppPaths = apps} -> res{incAppPaths = apps ++ [app]}
    
--
-- Set/reset the interactive flag.
--
makeInteractive :: Bool -> CmdArgs -> CmdArgs
makeInteractive i = makeCA $ \res -> res{interactive = i}

--
-- Raise an error due to an unknown option.
--
unknownOptError :: String -> CmdArgs
unknownOptError str = CmdError ("Undefined option: '" ++ str ++ "'")

--
-- Raise an error due to too few arguments to an option.
--
tooFewArgsError :: String -> Int -> Int -> CmdArgs
tooFewArgsError opt expArgs actArgs =
    CmdError ("Too few arguments for '" ++ opt ++ "'! "
              ++ "Expected " ++ show expArgs ++ ", got " ++ show actArgs ++ ".")

-------------------------------------------------------------------------------------------------------

--
-- Traverse the command-line options and either collect them in the
-- CmdArgs or flag an error in the same type.
--
parseArgs :: [String] -> CmdArgs -> CmdArgs
parseArgs (opt : code : args) cargs
    | opt `elem` ["--eval", "-e"]        = parseArgs args $ makeEval code cargs
    | opt `elem` ["--prelude"]           = parseArgs args $ makePrelude code cargs
    | opt `elem` ["-IA"]                 = parseArgs args $ makeIncAppPaths code cargs
    | opt `elem` ["-IP"]                 = parseArgs args $ makeIncPrePaths code cargs
parseArgs [opt] _
    | opt `elem` ["--eval", "-e"]        = tooFewArgsError opt 1 0
parseArgs (opt : args) cargs
    | opt `elem` ["--interactive", "-i"] = parseArgs args $ makeInteractive True  cargs
    | opt `elem` ["--batch",       "-b"] = parseArgs args $ makeInteractive False cargs
    | opt `elem` ["--version"          ] = CmdVersion
    | opt `elem` ["--help",        "-h"] = CmdUsage
parseArgs (err@('-':_) : _   ) _          = unknownOptError err
parseArgs (fName       : args) cargs      = parseArgs args $ makeImport fName cargs
parseArgs []                   cargs      = cargs

-------------------------------------------------------------------------------------------------------
--  Helper functions that give information to the user
-------------------------------------------------------------------------------------------------------

--
-- Print the usage message on stdout.
--
printUsage :: IO a
printUsage =
    do pName <- getProgName
       putStrLn $ usageStr pName
       exitWith ExitSuccess

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

-IA <path>                  Append <path> to STACKY_INCLUDE_PATH.
-IP <path>                  Prepend <path> to STACKY_INCLUDE_PATH.

--version                   Print the current version and terminate.

--help, -h                  Print this message and then terminate.

The interpreter will load all modules and execute the '--eval' options in the
order they appear on the command line. If in interactive mode, the REPL will
run after all loading is complete and in batch mode, the interpreter will
terminate once all is loaded and executed.

|]

-------------------------------------------------------------------------------------------------------

--
-- Print a command-line error message and terminate with error code 2.
--
printCmdError :: String -> IO a
printCmdError errMsg =
    do printErrorWithProgname (noPos, errMsg)
       exitWith (ExitFailure 2)
                
-------------------------------------------------------------------------------------------------------

--
-- Print the program version on stdout and terminate normally.
--
printVersion :: IO a
printVersion =
    do putStrLn $ "## Stacky - Copyright (c) 2024 Bengt Johansson -- All rights reserved"
       putStrLn $ "VERSION=" ++ version
       putStrLn $ "BUILD="   ++ build
       putStrLn $ "GIT_TAG=" ++ gitTag
       exitWith ExitSuccess

-------------------------------------------------------------------------------------------------------
--  That's all folks!!
-------------------------------------------------------------------------------------------------------
