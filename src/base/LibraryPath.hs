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
-- This module contains operations related to the STACKY_LIBRARY_PATH
--
-------------------------------------------------------------------------------------------------------

module LibraryPath (
                    loadLibPath,
                    findLibModule
                   ) where

-- System modules
import System.Environment(getExecutablePath, lookupEnv)
import System.Directory(doesFileExist)
import Data.List(intercalate)
import Data.List.Split(wordsBy)
import Data.List.Utils(split)

-- Local modules
import CoreTypes(Cxt(..))

-------------------------------------------------------------------------------------------------------
--  Definitions
-------------------------------------------------------------------------------------------------------

libPathEnv :: String
libPathEnv = "STACKY_LIBRARY_PATH"

-------------------------------------------------------------------------------------------------------
--  Main API
-------------------------------------------------------------------------------------------------------

--
-- Load the list of library paths from `libPathEnv`. If not defined, a
-- singleton element with the default prelude location is returned.
--

loadLibPath :: [String] -> [String] -> Cxt -> IO Cxt
loadLibPath pre app cxt =
    do res <- lookupEnv libPathEnv
       case res of
           Nothing ->
               do prelude <- makeDefaultPrelPath
                  return cxt{libPath = pre ++ [prelude] ++ app}
           Just str ->
               return cxt{libPath = pre ++ splitParts str ++ app}

--
-- Find a module in the libPath. If the name is a path to an existing
-- file, it is returned in verbatim, otherwise the libPath is searched
-- for a file modName.sy.
--
-- If no matching file is found, `Nothing` is returned.
--
findLibModule :: String -> Cxt -> IO (Maybe String)
findLibModule name Cxt{libPath = lPaths} =
    do isFileAlready <- doesFileExist name
       if isFileAlready then
           return $ Just name
       else
           lookupFile $ map (makePathName name) lPaths
                      
-------------------------------------------------------------------------------------------------------
--  Local helpers
-------------------------------------------------------------------------------------------------------

--
-- Split a colon separated list of paths into a list of paths
--
splitParts :: String -> [String]
splitParts = wordsBy (==':')

--
-- Make the default Prelude path.
--
-- Essentially "$pHome/../lib/$pName" where
--     pHome=$(dirname  "$0")
--     pName=$(basename "$0")
--
makeDefaultPrelPath :: IO String
makeDefaultPrelPath =
    do (pHome, pName) <- splitExecutableDir
       let prelude = pHome ++ "/../lib/" ++ pName ++ "/prelude"
       return $ prelude

--
-- Find the directory the executable is located in
--
-- Similar to BASH: dirname "$0"
--
splitExecutableDir :: IO (String, String)
splitExecutableDir =
    do path <- getExecutablePath
       return $ splitDirname path

--
-- Split a path into the executable file's name and leading path. Much
-- like `dirname` and `basename` in BASH.
--
splitDirname :: String -> (String, String)
splitDirname path = (dirName, exeName)
              where dirName = intercalate "/" $ init parts 
                    exeName = last parts
                    parts   = split "/" path


--
-- Search for the first file that exists in a list of filenames.
--
lookupFile :: [String] -> IO (Maybe String)
lookupFile []     = return Nothing
lookupFile (p:ps) = do isFileAlready <- doesFileExist p
                       if isFileAlready then
                           return $ Just p
                       else
                           lookupFile ps

--
-- Make a full file name from a path and a module name.
--
makePathName :: String -> String -> String
makePathName name p = p ++ "/" ++ name ++ ".sy"

-------------------------------------------------------------------------------------------------------
--  That's all folks!!
-------------------------------------------------------------------------------------------------------
