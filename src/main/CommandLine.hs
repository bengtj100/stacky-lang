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
                   parseArguments,
                   printGreeting
                   ) where

-- System modules
import Text.RawString.QQ
import System.Exit
import System.Environment

-- Base modules
import CoreTypes
import Position

-- Local modules
import Version

-------------------------------------------------------------------------------------------------------
--  Main API
-------------------------------------------------------------------------------------------------------

--
-- This is the type that collects information from command-line that
-- needs to be passed on to the rest of the program.
--

data CmdRes = CmdRes{ interactive :: Bool,       -- True = Interactive mode. Run the REPL
                      prelude     :: [Value],    -- Operations to run as the prelude.
                      preludeFile :: String }    -- Name of the Prelude file.

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

--
-- Print a startup greeting with Copyright, version etc.
--
printGreeting :: IO ()
printGreeting = printGreet

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
newCmdArgs = CmdArg CmdRes{interactive = True, prelude = [], preludeFile = ""}

--
-- Add commands to the prelude string to eval a string of code.
--
makeEval :: CmdArgs -> String -> CmdArgs
makeEval (CmdArg res@(CmdRes{prelude = p})) code =
    CmdArg res{prelude = p ++ [ValString noPos code, ValAtom noPos "eval"]}
makeEval _ _ =
    error "INTERNAL ERROR in CommandLine.makeEval"

--
-- Add commands to the prelude string to import a module.
--
makeImport :: CmdArgs -> String -> CmdArgs
makeImport (CmdArg (res@CmdRes{prelude = p})) fName
    = CmdArg res{prelude = p ++ [ValString noPos fName, ValAtom noPos "import"]}
makeImport _ _ =
    error "INTERNAL ERROR in CommandLine.makeImport"

--
-- Set the name of the prelude file to load the prelude from.
--
makePrelude :: CmdArgs -> String -> CmdArgs
makePrelude (CmdArg res) fName =
    CmdArg res{preludeFile = fName}
makePrelude _ _ =
    error "INTERNAL ERROR in CommandLine.makePrelude"

--
-- Set/reset the interactive flag.
--
makeInteractive :: CmdArgs -> Bool -> CmdArgs
makeInteractive (CmdArg res) i =
    CmdArg res{interactive = i}
makeInteractive _ _ =
    error "INTERNAL ERROR in CommandLine.makeInteractive"

--
-- Raise an error due to an unknown option.
--
unknownOptError :: String -> CmdArgs
unknownOptError str =
    CmdError ("Undefined option: '" ++ str ++ "'")

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

--
-- Print the startup greeting.
--
-- Helper function to `printGreeting` placed here because of likeness
-- to `printVersion`.
--
printGreet :: IO ()
printGreet =
    do putStrLn ""
       putStrLn $ "Stacky, version: " ++ version ++", build: " ++ build
       putStrLn $ "Copyright (c) 2024 Bengt Johansson -- All rights reserved"
       putStrLn ""

-------------------------------------------------------------------------------------------------------
--  That's all folks!!
-------------------------------------------------------------------------------------------------------
