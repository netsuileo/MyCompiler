module Main where

import Numeric(readInt, showIntAtBase)
import Data.Char
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad(foldM, filterM)
import Text.Regex.PCRE
import Text.Regex.PCRE.ByteString
import Text.Regex.PCRE.ByteString.Utils
import qualified Data.ByteString.Char8 as B(ByteString, pack, unpack)
import qualified Data.String.Utils as U(startswith, endswith, split) 

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
              deriving (Show, Eq)

class Identifyable a where
    getName :: a -> String
    getRegex :: a -> Regex

instance Identifyable Lexeme where
    getName LexError = "Error"
    getName LexAdd = "Add"
    getName LexMin = "Min"
    getName LexMul = "Mul" 
    getName LexDiv = "Div"
    getName LexMod = "Mod"
    getName LexEQ = "EQ"
    getName LexNE = "NE"
    getName LexLT = "LT"
    getName LexGT = "GT"
    getName LexLE = "LE"
    getName LexGE = "GE"
    getName LexLet = "Let"
    getName LexCast = "Cast"
    getName LexBeg = "Beg"
    getName LexEnd = "End"
    getName LexInt = "Int"
    getName LexReal = "Real"
    getName LexId = "Id"
    getName LexLRB = "LRB"
    getName LexRRB = "RRB"
    getName LexLSB = "LSB"
    getName LexRSB = "RSB"
    getName LexComma = "Comma"
    getName LexSemicolon = "Semicolon"
    getName LexColon = "Colon"
    getName LexVar = "Var"
    getName LexTypeInt = "TypeInt"
    getName LexTypeReal = "TypeReal"
    getName LexGoto = "Goto"
    getName LexRead = "Read"
    getName LexWrite = "Write"
    getName LexSkip = "Skip"
    getName LexSpace = "Space"
    getName LexTab = "Tab"
    getName LexTools = "Tools"
    getName LexProc = "Proc"
    getName LexCall = "Call"
    getName LexIf = "If"
    getName LexThen = "Then"
    getName LexElse = "Else"
    getName LexWhile = "While"
    getName LexDo = "Do"
    getRegex LexError = error "No regex for LexError"
    getRegex LexAdd = makeRegex "^\\+$" :: Regex 
    getRegex LexMin = makeRegex "^-$" :: Regex 
    getRegex LexMul = makeRegex "^\\*$" :: Regex 
    getRegex LexDiv = makeRegex "^/$" :: Regex 
    getRegex LexMod = makeRegex "(?i)^mod$" :: Regex 
    getRegex LexEQ = makeRegex "^=$" :: Regex 
    getRegex LexNE = makeRegex "^<>$" :: Regex 
    getRegex LexLT = makeRegex "^<$" :: Regex 
    getRegex LexGT = makeRegex "^>$" :: Regex 
    getRegex LexLE = makeRegex "^<=$" :: Regex 
    getRegex LexGE = makeRegex "^>=$" :: Regex 
    getRegex LexLet = makeRegex "^:=$" :: Regex 
    getRegex LexCast = makeRegex "(?i)^cast$" :: Regex 
    getRegex LexBeg = makeRegex "(?i)^begin$" :: Regex 
    getRegex LexEnd = makeRegex "(?i)^end$" :: Regex 
    getRegex LexInt = makeRegex "(?i)^[+-]?(2#[0-1]+|4#[0-3]+|8#[0-7]+|(10#)?[0-9]+|16#[0-9A-F]+)$" :: Regex 
    getRegex LexReal = makeRegex "(?i)^[-+]?([0-9]*\\.[0-9]+|[0-9]+\\.[0-9]*)([e][-+]?[0-9]+)?$" :: Regex 
    getRegex LexId = makeRegex "(?i)^[a-z]+[a-z0-9]*$" :: Regex 
    getRegex LexLRB = makeRegex "\\(" :: Regex 
    getRegex LexRRB = makeRegex "\\)" :: Regex 
    getRegex LexLSB = makeRegex "\\[" :: Regex 
    getRegex LexRSB = makeRegex "\\]" :: Regex 
    getRegex LexComma = makeRegex "\\," :: Regex 
    getRegex LexSemicolon = makeRegex "^;$" :: Regex 
    getRegex LexColon = makeRegex "^:$" :: Regex
    getRegex LexVar = makeRegex "(?i)^var$" :: Regex 
    getRegex LexTypeInt = makeRegex "(?i)^int$" :: Regex 
    getRegex LexTypeReal = makeRegex "(?i)^real$" :: Regex 
    getRegex LexGoto = makeRegex "(?i)^goto$" :: Regex 
    getRegex LexRead = makeRegex "(?i)^read$" :: Regex 
    getRegex LexWrite = makeRegex "(?i)^write$" :: Regex 
    getRegex LexSkip = makeRegex "(?i)^skip$" :: Regex 
    getRegex LexSpace = makeRegex "(?i)^space$" :: Regex 
    getRegex LexTab = makeRegex "(?i)^tab$" :: Regex 
    getRegex LexTools = makeRegex "(?i)^tools$" :: Regex 
    getRegex LexProc = makeRegex "(?i)^proc$" :: Regex 
    getRegex LexCall = makeRegex "(?i)^call$" :: Regex 
    getRegex LexIf = makeRegex "(?i)^if$" :: Regex 
    getRegex LexThen = makeRegex "(?i)^then$" :: Regex 
    getRegex LexElse = makeRegex "(?i)^else$" :: Regex 
    getRegex LexWhile = makeRegex "(?i)^while$" :: Regex 
    getRegex LexDo = makeRegex "(?i)^do$" :: Regex 

identifyFunctions = map checkForLexeme [LexAdd, LexMin, LexMul, LexDiv, LexMod,
                      LexEQ, LexNE, LexLT, LexGT, LexLE, LexGE,
                      LexComma, LexSemicolon, LexColon,
                      LexLRB, LexRRB, LexLSB, LexRSB,
                      LexLet, LexCast, LexBeg, LexEnd, LexInt, LexReal,
                      LexVar, LexTypeInt, LexTypeReal, LexGoto,
                      LexRead, LexWrite, LexSkip, LexSpace, LexTab,
                      LexTools, LexProc, LexCall,
                      LexIf, LexThen, LexElse,
                      LexWhile, LexDo,
                      LexId]

makeRegexForSubtitution :: (String, String) -> (Regex, B.ByteString)
makeRegexForSubtitution (line, sub) = (makeRegex line :: Regex, B.pack sub)

lexemsToSplit = map makeRegexForSubtitution [
                                            ("([^eE])\\+", "\\1 + \\2"),
                                            ("([^eE])-" , "\\1 - \\2"),
                                            ("\\*" , " * "), 
                                            ("/", " / "),
                                            ("([^:><])=", "\\1 = "), 
                                            ("([^<])>([^=])", "\\1 > \\2"), 
                                            ("<([^>=])", " < \\1"), 
                                            ("<>", " <> "), 
                                            (">=", " >= "), 
                                            ("<=", " <= "),
                                            (":=", " := "), 
                                            (";", " ; "), 
                                            (",", " , "),
                                            (":", " : "),
                                            ("\\[", " [ "), 
                                            ("\\]", " ] "), 
                                            ("\\(", " ( "), 
                                            ("\\)", " ) ")
                                            ]

comment = makeRegex ("\\{[^\\}]*\\}") :: Regex
commentBegin = makeRegex ("\\{[^\\}]*") :: Regex
commentEnd = makeRegex ("[^\\{]*\\}") :: Regex
separator = makeRegex "\\s+" :: Regex

getByte :: Either String B.ByteString -> B.ByteString
getByte (Left x) = B.pack x
getByte (Right x) = x

getByteList :: Either String [B.ByteString] -> [B.ByteString]
getByteList (Right x) = x

substituteLine :: B.ByteString -> (Regex, B.ByteString) -> IO B.ByteString
substituteLine line (regex, replacement) = do 
    result <- substitute regex line replacement
    return $ getByte (result)

checkForLexeme :: Lexeme -> B.ByteString  -> IO Lexeme
checkForLexeme lexeme str = do
    match <- execute (getRegex lexeme) str
    case match of
      Right (Just arr) -> return lexeme
      Right Nothing -> return LexError
      Left x -> error "42"
      
identify :: B.ByteString -> IO (Lexeme, B.ByteString)
identify str = do 
    let results = [lexeme | func <- identifyFunctions, let lexeme = unsafePerformIO (func str), lexeme /= LexError]
    if length results > 0 then return (results !! 0, str) else return (LexError, str)

baseAndIntSeparator = makeRegex "#" :: Regex

fromBase :: Int -> String -> Int
fromBase base = fst . head . readInt base ((<base).digitToInt) digitToInt

toBase :: Int -> Int -> String
toBase base num = showIntAtBase base intToDigit num ""

fromBaseToBase :: Int -> Int -> String -> String
fromBaseToBase from to = toBase to . fromBase from

intToStdForm :: String -> String
intToStdForm str = do
    let baseWithInt = getByteList $ unsafePerformIO (split baseAndIntSeparator  (B.pack str))
    if length baseWithInt == 1 then str
    else do 
      let base = read $ B.unpack $ baseWithInt !! 0 :: Int
      let int = B.unpack $ baseWithInt !! 1
      show $ fromBase base int

formatReal :: String -> String
formatReal str = do
    let splittedReal = U.split "." str
    if U.startswith "." str then
        "0" ++ str
    else if U.endswith "." str then
        str ++ "0"
    else if (length splittedReal == 2) && (U.startswith "e" (splittedReal !! 1)) then
        (splittedReal !! 0) ++ ".0" ++ (splittedReal !! 1)
    else
        str

realToStdForm :: String -> String 
realToStdForm str = show (read (formatReal str) :: Float)

makeOutputForLexeme :: (Lexeme, B.ByteString) -> String
makeOutputForLexeme (lexeme, value) = "lex:" ++ (getName lexeme) ++ "\t" ++
  (
    if lexeme == LexInt then 
      "int:" ++ (intToStdForm (B.unpack value)) ++ "\t"
    else if lexeme == LexReal then
      "real:" ++ (realToStdForm (B.unpack value)) ++ "\t"
    else ""
  ) ++ 
  "val:" ++ (B.unpack value) ++ "\n"

formOutput :: Integer -> [(Lexeme, B.ByteString)] -> String
formOutput lineNumber identified = do
    let linesList = map makeOutputForLexeme identified
    foldl (++) "" (map ((show lineNumber ++ "\t") ++ ) linesList)

processLine :: (Integer, String) -> IO String
processLine (lineNumber, line) = do
    lineWithSplittedLexems <- foldM substituteLine (B.pack line) lexemsToSplit
    lexems <- split separator lineWithSplittedLexems
    identifiedLexems <- mapM identify (filter (/= empty) (getByteList lexems))
    return (formOutput lineNumber identifiedLexems)

removeRegexFromLine :: Regex -> String -> IO B.ByteString
removeRegexFromLine regex line = substituteLine (B.pack line) (regex, B.pack "") 

removeOneLineComments :: String -> String
removeOneLineComments line = B.unpack (unsafePerformIO (removeRegexFromLine comment line))

doesRegexPresentInLine :: Regex -> B.ByteString -> IO Bool
doesRegexPresentInLine regex line = do
    match <- execute regex line
    case match of
      Right (Just arr) -> return True
      Right Nothing -> return False
      Left x -> error "42"

removeComments :: [(Integer, String)] -> Bool -> [(Integer, String)]
removeComments fileLines wasCommentOpened =
    if null fileLines then
      fileLines
    else do
      let lineNumber = fst (head fileLines)
      let line = removeOneLineComments (snd (head fileLines))
      if wasCommentOpened then
        if unsafePerformIO (doesRegexPresentInLine commentEnd (B.pack line)) then do 
          let lineWithoutComments = removeRegexFromLine commentEnd line
          (lineNumber, B.unpack (unsafePerformIO (lineWithoutComments))) : (removeComments (tail fileLines) False)
        else
          (lineNumber, "") : (removeComments (tail fileLines) True)
      else
        if unsafePerformIO (doesRegexPresentInLine commentBegin (B.pack line)) then do 
          let lineWithoutComments = removeRegexFromLine commentBegin line
          (lineNumber, B.unpack (unsafePerformIO (lineWithoutComments))) : (removeComments (tail fileLines) True)
        else
          (lineNumber, line) : (removeComments (tail fileLines) False)

getError :: B.ByteString -> String
getError line = do
    let splittedLine = getByteList (unsafePerformIO (split separator line))
    let lineNum = B.unpack $ splittedLine !! 0
    let lexemeType = B.unpack $ splittedLine !! 1
    if lexemeType == "lex:Error" then
      "Error:" ++ lineNum ++ ":" ++ "unrecognized lexeme\n"
    else if lexemeType == "lex:Real" then do
      let realVal = B.unpack $ splittedLine !! 2
      if realVal == "real:Infinity" then
        "Error:" ++ lineNum ++ ":" ++ "real value is too big\n"
      else ""
    else ""

getErrors :: String -> String
getErrors output = do
    let lines = getByteList (unsafePerformIO (split (makeRegex "\n" :: Regex) (B.pack output)))
    let errorOutputs = map getError lines
    foldl (++) "" (filter (/= "") errorOutputs)


main :: IO ()
main = do
    file <- readFile "a.txt"
    let fileLines = [(lineNumber, line) |
                    linesWithNumbers <- zip [1..] $ lines file,
                    let lineNumber = fst linesWithNumbers,
                    let line = (snd linesWithNumbers)]
    let linesToProcess = removeComments fileLines False 
    let result = mapM processLine linesToProcess
    mapM putStrLn (unsafePerformIO result)
    let errors = map getErrors (unsafePerformIO result)
    if length (filter (/= "") errors) == 0 then
      putStrLn "OK"
    else
      putStrLn (foldl (++) "" errors)
