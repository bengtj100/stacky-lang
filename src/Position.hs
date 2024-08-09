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

data Position = Pos{ fileName :: String, linePos :: Int, charPos :: Int }
              | EofPos
                deriving (Show, Read, Eq, Ord)

initPos :: String -> Position
initPos fname = Pos{fileName = fname, linePos = 0, charPos = 0}

eofPos :: Position
eofPos = EofPos

nextPos :: Position -> Char -> Position
nextPos p@Pos{linePos = l} '\n' = p{linePos = l + 1, charPos = 0}
nextPos p@Pos{}            '\r' = p{charPos = 0}
nextPos p@Pos{charPos = c} '\t' = p{charPos = calcTab c}
nextPos p@Pos{charPos = c} _    = p{charPos = c + 1}
nextPos EofPos             _    = EofPos

nextPosStr :: Position -> String -> Position
nextPosStr = foldl nextPos

incPosLine :: Position -> Int -> Position
incPosLine p@Pos{linePos = l} i = p{linePos = l + i}
incPosLine EofPos             _ = EofPos

incPosChar :: Position -> Int -> Position
incPosChar p@Pos{charPos = l} i = p{charPos = l + i}
incPosChar EofPos             _ = EofPos

calcTab :: Int -> Int
calcTab c = c + (8 - c `mod` 8)

noPos :: Position
noPos = Pos{fileName = "", linePos = -1, charPos = -1}

isNoPos :: Position -> Bool
isNoPos Pos{fileName = ""} = True
isNoPos _                  = False
                             
                                    
fmtPosition :: Position -> String
fmtPosition pos | isNoPos pos  = ""
                | otherwise    = fileName pos ++ ":"
                                 ++ show (linePos pos) ++ ":"
                                 ++ show (charPos pos) ++ ": "
