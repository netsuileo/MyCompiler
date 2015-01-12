{-# LANGUAGE LambdaCase #-}

module Parser where
import Lexeme
import Data.List.Split
import Data.Tree
import Control.Monad.Except
import Control.Monad.State

data InpLexeme = InpLexeme
     { lineNumber :: Integer
     , lexemeType :: Lexeme
     , numberValue :: String
     , lexemeValue :: String
     } deriving (Eq, Show)

data Program 
    = Program Variables Procedures Body
      deriving (Eq, Show)

type Variables = [VarDecl]

data VarDecl
    = VarDecl VarType [Var]
      deriving (Eq, Show)

data Var 
    = Var VarName (Maybe Size)
      deriving (Eq, Show)

type VarName = Id

data VarType 
    = TypeInt | TypeFloat
      deriving (Eq, Show)

type Size = String

type Procedures = [Proc]

data Proc
    = Proc ProcName Variables Body
      deriving (Eq, Show)

type ProcName = Id

data Body
    = Body Composite
    deriving (Eq, Show)

data Labeled 
    = Labeled Label Operator
    deriving (Eq, Show)

data Label
    = Label LabelName
    deriving (Eq, Show)

type LabelName = Id

type Id = String

data Composite
    = Composite [Operator]
      deriving (Eq, Show)

data Operator
    = OpEmpty
    | OpGoto LabelName
    | OpRead [VarName]
    | OpWrite [Writable]
    | OpVar Id (Maybe Index)
    | OpBreak
    | OpAssign VarName Expr
    | OpIf Expr [Operator] (Maybe [Operator])
    | OpWhile Expr [Operator]
      deriving (Show, Eq)

data Index 
    = IndexNum
    | VarName
    deriving (Eq, Show)

data Writable
    = WExpr Expr
    | WSpec Spec
      deriving (Show, Eq)

data Spec
    = SpecSkip | SpecSpace | SpecTab
      deriving (Show, Eq)

data Expr
    = ExprBinary BinOperation Operand Operand
    | ExprUnary UnOperation Operand
    | ExprOperand Operand
      deriving (Eq, Show)

data BinOperation
    = Multiply
    | Divide
    | Mod
    | Add
    | Minus
    | EQ
    | NE
    | GT
    | LT
    | GE
    | LE
      deriving (Eq, Show)

data UnOperation
    = Negation
      deriving (Eq, Show)

data Operand
    = OperandExpr
    | OperandVar
    | OperandInt
    | OperandFloat
      deriving (Eq, Show)

data ProcCall
    = ProcCall ProcName (Maybe [ProcVar])
      deriving (Eq, Show)

data ProcVar
    = ProcVar Id (Maybe Index)
      deriving (Eq, Show)

splitInputStringPart :: String -> (String, String)
splitInputStringPart str = do
    let parts = filter (/= "") (split (oneOf ":") str)
    (parts !! 0, parts !! 2)

stringToLexeme :: String -> InpLexeme
stringToLexeme str = do
    let parts = splitOn "\t" str
    let getLineNum = \part -> read part :: Integer
    let getLexemeType = \part -> getLexemeByName $ snd $ splitInputStringPart part
    let getLexemeValue = \part -> snd $ splitInputStringPart part
    let getNumberValue = \part -> snd $ splitInputStringPart part
    case length parts of
      4 -> do
        let lineNum = getLineNum $ parts !! 0
        let lexemeType = getLexemeType $ parts !! 1
        let numberValue = getNumberValue $ parts !! 2
        let lexemeValue = getLexemeValue $ parts !! 3
        InpLexeme lineNum lexemeType numberValue lexemeValue
      3 -> do
        let lineNum = getLineNum $ parts !! 0
        let lexemeType = getLexemeType $ parts !! 1
        let lexemeValue = getLexemeValue $ parts !! 2
        InpLexeme lineNum lexemeType "" lexemeValue
      otherwise -> error ("Wrong input string: \"" ++ str ++ "\"")

stringToLexemesList :: String -> [InpLexeme]
stringToLexemesList str = do
    map stringToLexeme (lines str)

data ParsingError
    = ExpectedError String
    | OtherError String
      deriving (Eq, Show)

parseVariables :: ExceptT ParsingError (State [InpLexeme]) Variables
parseVariables = get >>= \case
    [] -> return []
    InpLexeme _ LexTools _ _ : _ -> 
        modify (drop 1) >> parseVariableDeclarations >>= return
    otherwise -> return []

parseVariableDeclarations :: ExceptT ParsingError (State [InpLexeme]) Variables
parseVariableDeclarations = do
    decl <- parseVariableDeclaration
    state <- get
    case state of
      InpLexeme _ LexSemicolon _ _ : _ ->
        modify (drop 1) >>
        parseVariableDeclaration >>= 
        return . ([decl] ++) . (\x -> [x])
      otherwise ->      
        return [decl]

parseVariableDeclaration :: ExceptT ParsingError (State [InpLexeme]) VarDecl
parseVariableDeclaration = get >>= \case
    [] -> throwError $ ExpectedError "int or real"
    InpLexeme _ LexTypeInt _ _ : _ ->
      modify (drop 1) >>
      parseVarChain >>=
      return . VarDecl TypeInt
    InpLexeme _ LexTypeReal _ _ : _ ->
      modify (drop 1) >>
      parseVarChain >>=
      return . VarDecl TypeFloat
    otherwise -> throwError $ ExpectedError "int or real"

parseVarChain :: ExceptT ParsingError (State [InpLexeme]) [Var]
parseVarChain = do
    state <- get
    case state of
      [] -> throwError $ ExpectedError "identifier"
      InpLexeme _ LexId _ n : InpLexeme _ LexInt v _ : InpLexeme _ LexComma _ _ : _->
        modify (drop 3) >> parseVarChain >>= return . ([Var n $ Just v] ++)
      InpLexeme _ LexId _ n : InpLexeme _ LexComma  _ _ : _ ->
        modify (drop 2) >> parseVarChain >>= return . ([Var n Nothing] ++)
      InpLexeme _ LexId _ n : InpLexeme _ LexInt v _ : _ ->
        modify (drop 2) >> return [Var n $ Just v]
      InpLexeme _ LexId _ n : _ ->
        modify (drop 1) >> return [Var n Nothing]
      otherwise -> throwError $ ExpectedError "identifier"

parseProcedures :: ExceptT ParsingError (State [InpLexeme]) Procedures
parseProcedures = get >>= \case
    [] -> return []
    InpLexeme _ LexProc _ _ : _ -> 
        modify (drop 1) >> parseProcedureDeclarations >>= return
    otherwise -> return []

parseProcedureDeclarations :: ExceptT ParsingError (State [InpLexeme]) Procedures
parseProcedureDeclarations = do
    decl <- parseProcedureDeclaration
    state <- get
    case state of
      InpLexeme _ LexSemicolon _ _ : _ ->
        modify (drop 1) >>
        parseProcedureDeclaration >>= 
        return . ([decl] ++) . (\x -> [x])
      otherwise ->
        return [decl]

parseProcedureDeclaration :: ExceptT ParsingError (State [InpLexeme]) Proc
parseProcedureDeclaration = get >>= \case
    [] -> throwError $ ExpectedError "proc"
    InpLexeme _ LexProc _ _ : InpLexeme _ LexId _ n : _ -> do
      modify (drop 2)
      variables <- parseVariableDeclarations
      body <- parseBody
      return $ Proc n variables body
    otherwise -> throwError $ ExpectedError "proc"

parseBody :: ExceptT ParsingError (State [InpLexeme]) Body
parseBody = return $ Body $ Composite [] 

parse :: ExceptT ParsingError (State [InpLexeme]) Program
parse = do
    variables <- parseVariables
    procedures <- parseProcedures
    body <- parseBody
    return $ Program variables procedures body

startParsing :: [InpLexeme] -> Either ParsingError Program
startParsing = fst . runState (runExceptT parse)

runParse :: String -> String
runParse input = do
    let lexemes = stringToLexemesList input
        a = startParsing lexemes
    show a
