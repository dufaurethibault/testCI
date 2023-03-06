module MarcelDef (
    Value(..)
    , Expression(..)
    , Statement(..)
    , Binop(..)
    , FunctionCall(..)
    , Function(..)
    , IfStatement(..)
    , ElseIfStatement(..)
    , ElseStatement(..)
    , WhileStatement(..)
    , Program(..)
    , VarDeclaration(..)
    , MarcelToken(..)
    , ForStatement(..)
    , MarcelStringParser(..)
    , MarcelStringParserResult(..)
) where

{-# OPTIONS_GHC -WEverything #-}

data Function = Function {
    fname :: String,
    freturn :: String,
    fargs :: [String],
    fstatements :: [Statement]
} deriving (Show, Eq)

data IfStatement = IfStatement {
    s_condition :: Expression,
    s_statements :: [Statement],
    s_elseif :: Maybe ElseIfStatement,
    s_else :: Maybe ElseStatement
} deriving (Show, Eq)

data ElseIfStatement = ElseIfStatement {
    ss_condition :: Expression,
    ss_statements :: [Statement],
    ss_elseif :: Maybe ElseIfStatement,
    ss_else :: Maybe ElseStatement
} deriving (Show, Eq)

data ElseStatement = ElseStatement {
    sn_statements :: [Statement]
} deriving (Show, Eq)

data WhileStatement = WhileStatement {
    tk_condition :: Expression,
    tk_statements :: [Statement]
} deriving (Show, Eq)

data ForStatement = ForStatement {
    f_start :: Statement,
    f_cond :: Expression,
    f_end :: Expression,
    f_statements :: [Statement]
} deriving (Show, Eq)

data Value = VSymbol String |
            VChar Char |
            VNumber Int |
            VFloating Float |
            VString String |
            VBool Bool |
            VParen Expression
            deriving (Show, Eq)

data FunctionCall = FunctionCall {
    fc_name :: String,
    fc_expressions :: [Expression]
} deriving (Show, Eq)

data VarDeclaration = VarDeclaration {
    vd_name :: String,
    vd_type :: String,
    vd_value :: Expression
} deriving (Show, Eq)

data Binop =
    BPlus |
    BMinus |
    BMul |
    BDivide |
    BModulo |
    BEquals |
    BNotEquals |
    BLessThan |
    BGreaterThan |
    BLessThanEquals |
    BGreaterThanEquals |
    BAssing
    deriving (Show, Eq, Ord)

data Expression = EValue Value |
                  EFunction FunctionCall |
                  EBinop Expression Binop Expression |
                  EBreak |
                  ENone
                  deriving (Show, Eq)

data Statement = SIfStatement IfStatement |
                 SElseIfStatement ElseIfStatement |
                 SElseStatement ElseStatement |
                 SWhileStatement WhileStatement |
                 SForStatement ForStatement |
                 SDeclaration VarDeclaration |
                 SReturn Expression |
                 SExpression Expression
                 deriving (Show, Eq)

data Program = PFunction Function |
               PStatement Statement
               deriving (Show, Eq)

type MarcelStringParserResult = Either (String, String) [String]

data MarcelToken =  TSymbol String |
                    TNumber Int |
                    TChar Char |
                    TString String |
                    TBinop Binop |
                    TBool Bool |
                    TLeftParen |
                    TRightParen |
                    TLeftBrace |
                    TRightBrace |
                    TLeftBracket |
                    TRightBracket |
                    TComma |
                    TDot |
                    TIf |
                    TElseIf |
                    TElse |
                    TWhile |
                    TFor |
                    TSemicolon |
                    TFuncDec |
                    TFunc |
                    TBegin |
                    TReturn |
                    TEnd |
                    TBreak
                    deriving (Show, Eq)

type MarcelStringParser = (String -> MarcelStringParserResult)
