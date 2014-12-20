module Main where

import Prelude 
import System.Environment(getArgs)
import Numeric(readInt, showIntAtBase, showEFloat) 
import Data.Char(isSpace, isDigit, isAlpha, isAlphaNum, digitToInt, intToDigit, toLower)
import Data.List(elem, last, isInfixOf)
import Data.List.Split(splitOn, splitOneOf)
import Debug.Trace(trace)

stringToLower :: String -> String
stringToLower str = map toLower str

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

data LexemeOut = LexemeNumber { lineNum :: Integer,
                                lexemeType :: Lexeme,
                                interpretedValue :: String,
                                value :: String } |
                 LexemeError  { lineNum :: Integer,
                                lexemeType :: Lexeme,
                                message :: String,
                                value :: String } |
                 LexemeOther  { lineNum :: Integer,
                                lexemeType :: Lexeme,
                                value :: String }

instance Eq LexemeOut where
    LexemeNumber _ _ _ _ == LexemeNumber _ _ _ _ = True
    LexemeError _ _ _ _ == LexemeError _ _ _ _ = True
    LexemeOther _ _ _ == LexemeOther _ _ _ = True
    _ == _ = False

class ShowError a where
    showError :: a -> String

instance ShowError LexemeOut where
    showError LexemeError {lineNum = n, lexemeType = t, message = m, value = v} =
      "Error:" ++ show n ++ ":" ++ m
    showError _ = error "ShowError works only with LexemeError type"

instance Show LexemeOut where
    show LexemeNumber {lineNum = n, lexemeType = t, interpretedValue = i, value = v} =
      show n ++ "\tLex:" ++ show t ++
      (if t == LexInt then "\tint:" else "\treal:") ++ i ++ "\tval:" ++ v
    show LexemeError  {lineNum = n, lexemeType = t, message = m, value = v} =
      show n ++ "\tLex:" ++ show t ++ "\tval:" ++ v
    show LexemeOther  {lineNum = n, lexemeType = t, value = v} =
      show n ++ "\tLex:" ++ show t ++ "\tval:" ++ v

isSeparator :: Char -> Bool
isSeparator c = isSpace c || c `elem` "+-*/()[]{}=><,;:"

removeComment :: Integer -> String -> (Integer, String)
removeComment lineNum (c:str)
  | c == '}' = (lineNum, str)
  | c == '\n' = removeComment (lineNum + 1) str
  | otherwise = removeComment lineNum str

makeUnrecognizedLexeme :: Integer -> String -> String -> (LexemeOut, String)
makeUnrecognizedLexeme lineNum (c:str) lexStr
  | lexStr == "" = makeUnrecognizedLexeme lineNum str (lexStr ++ [c])
  | isSeparator c = (LexemeError lineNum LexError "Unrecognized lexeme" lexStr, c:str)
  | str == [] = (LexemeError lineNum LexError "Unrecognized lexeme" (lexStr ++ [c]), str)
  | otherwise = makeUnrecognizedLexeme lineNum str (lexStr ++ [c])

isOneSymbolLexeme :: Char -> Bool
isOneSymbolLexeme c = c `elem` "+-*/()[]=><,;:"

getOneSymbolLexemeType :: Char -> Lexeme
getOneSymbolLexemeType c
  | c == '+' = LexAdd
  | c == '-' = LexMin
  | c == '*' = LexMul
  | c == '/' = LexDiv
  | c == '(' = LexRRB
  | c == ')' = LexLRB
  | c == '[' = LexRSB
  | c == ']' = LexLSB
  | c == '=' = LexEQ
  | c == '>' = LexGT
  | c == '<' = LexLT
  | c == ',' = LexComma
  | c == ';' = LexSemicolon
  | c == ':' = LexColon
  | otherwise = error ("No lexeme type for " ++ [c] ++ " symbol")

makeOneSymbolLexeme :: Integer -> Char -> LexemeOut
makeOneSymbolLexeme lineNum c = 
    LexemeOther lineNum (getOneSymbolLexemeType c) [c]

getTwoSymbolLexemeType :: Char -> Char -> Lexeme
getTwoSymbolLexemeType c1 c2 
  | c1 == '>' && c2 == '=' = LexGE
  | c1 == '<' && c2 == '=' = LexLE
  | c1 == '<' && c2 == '>' = LexNE
  | c1 == ':' && c2 == '=' = LexLet
  | otherwise = error ("No lexeme type for" ++ [c1, c2] ++ " symbol")

isTwoSymbolLexeme :: Char -> Char -> Bool
isTwoSymbolLexeme c1 c2 =
    (c1 == '>' && c2 == '=') ||
    (c1 == '<' && (c2 == '=' || c2 == '>')) ||
    (c1 == ':' && c2 == '=')

makeTwoSymbolLexeme :: Integer -> Char -> Char -> LexemeOut
makeTwoSymbolLexeme lineNum c1 c2 = 
    LexemeOther lineNum (getTwoSymbolLexemeType c1 c2) [c1, c2]

getKeywordOrIdLexemeType :: String -> Lexeme
getKeywordOrIdLexemeType str
  | lowerstr == "mod" = LexMod
  | lowerstr == "cast" = LexCast
  | lowerstr == "begin" = LexBeg
  | lowerstr == "end" = LexEnd
  | lowerstr == "var" = LexVar
  | lowerstr == "int" = LexTypeInt
  | lowerstr == "real" = LexTypeReal
  | lowerstr == "goto" = LexGoto
  | lowerstr == "read" = LexRead
  | lowerstr == "write" = LexWrite
  | lowerstr == "skip" = LexSkip
  | lowerstr == "space" = LexSpace
  | lowerstr == "tab" = LexTab
  | lowerstr == "tools" = LexTools
  | lowerstr == "proc" = LexProc
  | lowerstr == "call" = LexCall
  | lowerstr == "if" = LexIf
  | lowerstr == "then" = LexThen
  | lowerstr == "else" = LexElse
  | lowerstr == "while" = LexWhile
  | lowerstr == "do" = LexDo
  | otherwise = LexId
  where lowerstr = stringToLower str

makeKeywordOrIdLexeme :: Integer -> String -> String -> (LexemeOut, String)
makeKeywordOrIdLexeme lineNum (c:str) lexStr 
  | lexStr == "" = makeKeywordOrIdLexeme lineNum str (lexStr ++ [c])
  | str == [] && isAlphaNum c = (LexemeOther lineNum (getKeywordOrIdLexemeType (lexStr ++ [c])) (lexStr ++ [c]), str)
  | isAlphaNum c = makeKeywordOrIdLexeme lineNum str (lexStr ++ [c])
  | isSeparator c = (LexemeOther lineNum (getKeywordOrIdLexemeType lexStr) lexStr, c:str)
  | otherwise = makeUnrecognizedLexeme lineNum str (lexStr ++ [c])

fromBase :: Int -> String -> Int
fromBase base = fst . head . readInt base ((<base).digitToInt) digitToInt

parseInteger :: Integer -> String -> LexemeOut
parseInteger lineNum lexStr = do
    let baseAndNum = splitOn "#" lexStr
    if length baseAndNum == 2 then do
      if baseAndNum !! 1 == [] then
        LexemeError lineNum LexError "Number lexeme is unfinished" lexStr
      else do
        let baseStr = baseAndNum !! 0
        let numStr = baseAndNum !! 1
        if baseStr `elem` ["2", "4", "8", "10", "16"] then do
          let base = read baseStr :: Int
          if foldr (&&) True (map ((<base).digitToInt) numStr) then
            LexemeNumber lineNum LexInt (show $ fromBase base numStr) lexStr
          else
            LexemeError lineNum LexError "Wrong number in current base" lexStr
        else
          LexemeError lineNum LexError "Wrong base number" lexStr
    else if length baseAndNum == 1 then do
      let numStr = baseAndNum !! 0
      let num = read numStr :: Int
      LexemeNumber lineNum LexInt (show num) lexStr
    else
      LexemeError lineNum LexError "Unrecognized lexeme" lexStr

formatReal :: String -> String
formatReal str = do
    let splittedReal = splitOn "." str
    if head str == '.' then
      "0" ++ str
    else if last str == '.' then
      str ++ "0"
    else if length splittedReal == 2 && (head (splittedReal !! 1)) `elem` "eE" then do
      (splittedReal !! 0) ++ ".0" ++ (splittedReal !! 1)
    else
      str

parseFloat :: Integer -> String -> LexemeOut
parseFloat lineNum lexStr = do
    let formatted = formatReal lexStr
    let float = read formatted :: Float
    if (show float) `elem` ["Infinity", "-Infinity"] then
      LexemeError lineNum LexError "Float value is too big" lexStr
    else do
      let floatRepr = showEFloat Nothing float ""
      if not $ "e-" `isInfixOf` floatRepr then do
        let splittedFloat = splitOneOf "e" floatRepr
        LexemeNumber lineNum LexReal (splittedFloat !! 0 ++ "e+" ++ splittedFloat !! 1) lexStr
      else
        LexemeNumber lineNum LexReal floatRepr lexStr

makeFloatLexeme :: Integer -> String -> String -> (LexemeOut, String)
makeFloatLexeme lineNum (c:str) lexStr
  | c `elem` ".eE" = if c `elem` lexStr then
                      makeUnrecognizedLexeme lineNum str (lexStr ++ [c])
                    else
                      makeFloatLexeme lineNum str (lexStr ++ [c])
  | c `elem` "+-" = if '+' `elem` lexStr || '-' `elem` lexStr then
                      makeUnrecognizedLexeme lineNum str (lexStr ++ [c])
                    else if (last lexStr) `elem` "eE" then
                      makeFloatLexeme lineNum str (lexStr ++ [c]) 
                    else
                      makeUnrecognizedLexeme lineNum str (lexStr ++ [c])
  | isDigit c = makeFloatLexeme lineNum str (lexStr ++ [c]) 
  | isSeparator c = (parseFloat lineNum lexStr, c:str)
  | otherwise = makeUnrecognizedLexeme lineNum str (lexStr ++ [c])

makeIntegerLexeme :: Integer -> String -> String -> (LexemeOut, String)
makeIntegerLexeme lineNum (c:str) lexStr
  | c == '#' = if c `elem` lexStr then
                 makeUnrecognizedLexeme lineNum str (lexStr ++ [c])
               else
                 makeIntegerLexeme lineNum str (lexStr ++ [c])
  | c `elem` "1234567890aAbBcCdDeEfF" = makeIntegerLexeme lineNum str (lexStr ++ [c])
  | isSeparator c = (parseInteger lineNum lexStr, c:str)
  | otherwise = makeUnrecognizedLexeme lineNum str (lexStr ++ [c])

makeNumberLexeme :: Integer -> String -> String -> (LexemeOut, String)
makeNumberLexeme lineNum (c:str) lexStr
  | isDigit c = makeNumberLexeme lineNum str (lexStr ++ [c]) 
  | c == '.' || c `elem` "eE" = makeFloatLexeme lineNum str (lexStr ++ [c])
  | c == '#' = makeIntegerLexeme lineNum str (lexStr ++ [c])
  | str == [] = (parseInteger lineNum lexStr, c:str)
  | isSeparator c = (parseInteger lineNum lexStr, c:str)
  | otherwise = makeUnrecognizedLexeme lineNum str (lexStr ++ [c])

getLexemes :: Integer -> String -> [LexemeOut]
getLexemes _ [] = []
getLexemes lineNum (c:str) =
    if c == '{' then do
      let withoutComment = removeComment lineNum str
      getLexemes (fst withoutComment) (snd withoutComment)
    else if str /= [] && isTwoSymbolLexeme c (head str) then
      makeTwoSymbolLexeme lineNum c (head str) : getLexemes lineNum (tail str)
    else if isOneSymbolLexeme c then
      makeOneSymbolLexeme lineNum c : getLexemes lineNum str
    else if isDigit c || c == '.' then do
      let lexemeAndStrTail = makeNumberLexeme lineNum (c:str) ""
      (fst lexemeAndStrTail) : getLexemes lineNum (snd lexemeAndStrTail)
    else if isAlpha c then do
      let lexemeAndStrTail = makeKeywordOrIdLexeme lineNum (c:str) ""
      (fst lexemeAndStrTail) : getLexemes lineNum (snd lexemeAndStrTail)
    else if c == '\n' then
      getLexemes (lineNum + 1) str
    else if isSpace c then
      getLexemes lineNum str
    else do
      let withoutUnrecognized = makeUnrecognizedLexeme lineNum (c:str) ""
      (fst withoutUnrecognized) : getLexemes lineNum (snd withoutUnrecognized)

main :: IO ()
main = do
    args <- getArgs
    if length args < 2 then
      putStrLn "Error:Params:too few arguments"
    else do
      file <- readFile (args !! 0)
      let lexemes = getLexemes 1 file
      let errors = filter (== LexemeError 0 LexError "" "") lexemes
      writeFile (args !! 1) (foldr (++) "" (map (++ "\n") (map show lexemes)))
      if length errors == 0 then
        putStrLn "OK"
      else do
        let errorMessages = map (++ "\n") (map showError errors)
        let errorStr = foldr (++) "" errorMessages
        putStr errorStr
