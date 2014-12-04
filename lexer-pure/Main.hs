module Main where

import Prelude 
import System.Environment(getArgs)
import Data.Char(isSpace)
import Data.List(isInfixOf)
import Debug.Trace(trace)

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
      show n ++ ":\tLex:" ++ show t ++ "\tval:" ++ v
    show LexemeError  {lineNum = n, lexemeType = t, message = m, value = v} =
      show n ++ ":\tLex:" ++ show t ++ "\tval:" ++ v
    show LexemeOther  {lineNum = n, lexemeType = t, value = v} =
      show n ++ ":\tLex:" ++ show t ++ "\tval:" ++ v

isSeparator :: Char -> Bool
-- isSeparator c = isInfixOf [c] "\t\n\r\f\v +-*/()[]{}=><,;:"
isSeparator c = isInfixOf [c] "\t\n\r\f\v"

performComment :: Integer -> String -> (Integer, String)
performComment lineNum (c:str)
  | c == '}' = (lineNum, str)
  | c == '\n' = performComment (lineNum + 1) str
  | otherwise = performComment lineNum str

performUnrecognizedLexeme :: Integer -> String -> String -> (LexemeOut, String)
performUnrecognizedLexeme lineNum (c:str) lexStr
  | str == [] = (LexemeError lineNum LexError "UnrecognizedLexeme" (c:lexStr), str)
  | isSeparator c = (LexemeError lineNum LexError "UnrecognizedLexeme" lexStr, c:str)
  | otherwise = performUnrecognizedLexeme lineNum str (c:lexStr)

getLexemes :: Integer -> String -> [LexemeOut]
getLexemes _ [] = []
getLexemes lineNum (c:str) =
    if c == '{' then do
      let withoutComment = performComment lineNum str
      getLexemes (fst withoutComment) (snd withoutComment)
    else if c == ':' then
      LexemeOther lineNum LexColon [c] : getLexemes lineNum str
    else if c == '\n' then
      getLexemes (lineNum + 1) str
    else if isSpace c then
      getLexemes lineNum str
    else do
      let withoutUnrecognized = performUnrecognizedLexeme lineNum (c:str) ""
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
