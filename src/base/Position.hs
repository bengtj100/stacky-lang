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
-- This module implements a data-type that represents positions in
-- files. It contains:
--  - A file name
--  - A line position
--  - A character position on a given line.
--
-- Positions start on zero (0).
-- There is also a marker for end-of-file (EOF).
--
-------------------------------------------------------------------------------------------------------

module Position (
                  Position(..),
                  initPos, eofPos,
                  nextPos,
                  nextPosStr,
                  incPosLine,
                  incPosChar,
                  noPos, isNoPos,
                  fmtPosition
                 ) where
    
-------------------------------------------------------------------------------------------------------
--  Main API
-------------------------------------------------------------------------------------------------------

data Position = Pos{ fileName :: String, linePos :: Int, charPos :: Int }
              | EofPos
                deriving (Show, Read, Eq, Ord)

--
-- Initialize a position to the first character of the first line in a file.
--
initPos :: String -> Position
initPos fname = Pos{fileName = fname, linePos = 0, charPos = 0}

--
-- Initialize a position to the EOF marker.
--
eofPos :: Position
eofPos = EofPos

--
-- Move one character forward
--
nextPos :: Position -> Char -> Position
nextPos p@Pos{linePos = l} '\n' = p{linePos = l + 1, charPos = 0}
nextPos p@Pos{}            '\r' = p{charPos = 0}
nextPos p@Pos{charPos = c} '\t' = p{charPos = calcTab c}
nextPos p@Pos{charPos = c} _    = p{charPos = c + 1}
nextPos EofPos             _    = EofPos

--
-- Move several characters forward
--
nextPosStr :: Position -> String -> Position
nextPosStr = foldl nextPos

--
-- Go to the next line
--
incPosLine :: Position -> Int -> Position
incPosLine p@Pos{linePos = l} i = p{linePos = l + i}
incPosLine EofPos             _ = EofPos

--
-- Go to the next character
--
incPosChar :: Position -> Int -> Position
incPosChar p@Pos{charPos = l} i = p{charPos = l + i}
incPosChar EofPos             _ = EofPos

--
-- This a marker showing that no position is available
--
noPos :: Position
noPos = Pos{fileName = "", linePos = -1, charPos = -1}

--
-- Test if a position is equal to noPos
--
isNoPos :: Position -> Bool
isNoPos pos = pos == noPos
                             
--
-- Nicely format a position as a string
--
fmtPosition :: Position -> String
fmtPosition EofPos               = "EOF: "
fmtPosition p@Pos{fileName = ""} = fmtPosition p{fileName = "-"}
fmtPosition pos                  = fileName pos ++ ":"
                                   ++ show (linePos pos) ++ ":"
                                   ++ show (charPos pos) ++ ": "

-------------------------------------------------------------------------------------------------------
--  Helper functions
-------------------------------------------------------------------------------------------------------

--
-- Calculate a jump to the next 8 character tab.
--
calcTab :: Int -> Int
calcTab c = c + (8 - c `mod` 8)

-------------------------------------------------------------------------------------------------------
--  That's all folks!!
-------------------------------------------------------------------------------------------------------
