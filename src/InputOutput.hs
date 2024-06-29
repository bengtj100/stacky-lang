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

module InputOutput (getLines) where
                     
import System.IO

-- ====================================================================================================

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

