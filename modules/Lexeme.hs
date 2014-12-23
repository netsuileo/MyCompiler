module Lexeme where

data Lexeme = LexError | 
              LexAdd | LexMin| LexMul | LexDiv | LexMod |
              LexEQ | LexNE | LexLT | LexGT | LexLE | LexGE |
              LexLet | LexCast |
              LexBeg | LexEnd |
              LexInt | LexReal | 
              LexId |
              LexLRB | LexRRB | LexLSB | LexRSB |
              LexComma | LexSemicolon | LexColon |
              LexVar | LexTypeInt | LexTypeReal | LexGoto |
              LexRead | LexWrite | LexSkip | LexSpace | LexTab |
              LexTools | LexProc | LexCall |
              LexIf | LexThen | LexElse |
              LexWhile | LexDo
              deriving (Eq)

instance Show Lexeme where
    show LexError = "Error"
    show LexAdd = "Add"
    show LexMin = "Min"
    show LexMul = "Mul" 
    show LexDiv = "Div"
    show LexMod = "Mod"
    show LexEQ = "EQ"
    show LexNE = "NE"
    show LexLT = "LT"
    show LexGT = "GT"
    show LexLE = "LE"
    show LexGE = "GE"
    show LexLet = "Let"
    show LexCast = "Cast"
    show LexBeg = "Beg"
    show LexEnd = "End"
    show LexInt = "Int"
    show LexReal = "Real"
    show LexId = "Id"
    show LexLRB = "LRB"
    show LexRRB = "RRB"
    show LexLSB = "LSB"
    show LexRSB = "RSB"
    show LexComma = "Comma"
    show LexSemicolon = "Semicolon"
    show LexColon = "Colon"
    show LexVar = "Var"
    show LexTypeInt = "TypeInt"
    show LexTypeReal = "TypeReal"
    show LexGoto = "Goto"
    show LexRead = "Read"
    show LexWrite = "Write"
    show LexSkip = "Skip"
    show LexSpace = "Space"
    show LexTab = "Tab"
    show LexTools = "Tools"
    show LexProc = "Proc"
    show LexCall = "Call"
    show LexIf = "If"
    show LexThen = "Then"
    show LexElse = "Else"
    show LexWhile = "While"
    show LexDo = "Do"
