module Parser where
import Lexeme
import Data.Tree

data InpLexeme = InpLexeme { lineNum :: Integer,
                           lexemeType :: Lexeme,
                           lexemeValue :: String,
                           numberValue :: String }
                           deriving (Eq)

data NumberType = IntType | RealType

data OperandType = Empty

data QualifierType = Tab | Skip | Space

data EntryType = Dummy

data XmlTag = Program |
              Procedure | 
              Call |
              Label String |
              Definition String Integer NumberType | 
              Brief String Integer | 
              Clause |
              Compound | 
              Assign | 
              Cast |
              Goto String |
              If | 
              While | 
              Read | 
              Write | 
              Variable String Integer |
              IntegerLiteral String | 
              RealLiteral String | 
              Expression |
              Operand OperandType | 
              Qualifier QualifierType |
              Table | 
              Entry String EntryType Integer Integer

class TreeToXml a where
    toXml :: Tree a -> String

instance TreeToXml XmlTag where
    toXml tree = "Lol"

processProgram :: [InpLexeme] -> Tree XmlTag
processProgram lexemes = Node Program []

stringToLexeme :: String -> InpLexeme
stringToLexeme str = do
    let splittedStr = splitOn '\t' str


stringToLexemesList :: String -> [InpLexeme]
stringToLexemesList str = do
    map stringToLexeme (lines str)

parse :: String -> String
parse input = do
    let lexemes = stringToLexemesList input
    let tree = processProgram lexemes
    toXml tree
