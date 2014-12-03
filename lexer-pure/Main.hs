module Main where

import Prelude 
import System.Environment(getArgs)

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

getLexemes :: String -> [LexemeOut]
getLexemes file = do

main :: IO ()
main = do
    args <- getArgs
    if length args < 2 then
      putStrLn "Error:Params:too few arguments"
    else do
      file <- readFile (args !! 0)
      let lexemes = getLexemes file
      let errors = filter (== LexemeError 0 LexError "" "") lexemes
      writeFile (args !! 1) (foldr (++) "" (map (++ "\n") (map show lexemes)))
      if length errors == 0 then
        putStrLn "OK"
      else do
        let errorMessages = map (++ "\n") (map showError errors)
        let errorStr = foldr (++) "" errorMessages
        putStr errorStr
