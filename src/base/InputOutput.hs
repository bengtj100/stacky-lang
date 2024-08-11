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
-- This module contains I/O helper functions
--
-------------------------------------------------------------------------------------------------------

module InputOutput (getLines) where

-- System modules
import System.IO(hFlush, stdout)

-------------------------------------------------------------------------------------------------------
--  Main API
-------------------------------------------------------------------------------------------------------

--
-- Read one or more lines from stdin to a string.
--
-- It takes a prompt string, prints it and reads a line from stdin. If
-- the line ends with a back-slash '\' it reprints the prompt and lets
-- you continue entering text. This continues as long as the user
-- terminates each line with a '\'.
--
getLines :: String -> IO String
getLines prompt =
    do putStr prompt
       hFlush stdout
       line <- getLine
       case line of
           "" ->
               return ""
           _ | last line == '\\' ->
               do putStr " ... "
                  allLines <- getLines prompt
                  return $ (init line) ++ "\n" ++ allLines
             | otherwise ->
               return line

-------------------------------------------------------------------------------------------------------
--  That's all folks!!
-------------------------------------------------------------------------------------------------------
