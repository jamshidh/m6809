
module Format where

import System.Console.ANSI

class Format a where
  format::a->String


box::[String]->[String]
box lines' =
    ["╔" ++ replicate maxLength '═' ++ "╗" ++ clearFromCursorToLineEndCode]
    ++ map (\l -> "║" ++ l ++ replicate (maxLength - length l) ' ' ++ "║" ++ clearFromCursorToLineEndCode) lines'
    ++ ["╚" ++ replicate maxLength '═' ++ "╝" ++ clearFromCursorToLineEndCode]

  where
    maxLength = maximum $ map length lines'

tab::[String]->[String]
tab lines' = map ("  " ++) lines'
     
