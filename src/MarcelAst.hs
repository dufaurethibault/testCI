module MarcelAst (
    marcelFromFile,
    expression_as_list,
    expression_list_as_tree,
    ExprTree(..),
    BTree (..),
    marcelTokensAsFunc,
    marcelTokensAsFuncArgs',
    marcelTokensAsFuncArgs,
    marcelTokensAsFuncBody,
    marcelAsAst
) where

{-# OPTIONS_GHC -WEverything #-}

import MarcelDef
import MarcelParser
import System.IO

type MarcelAstResult a = Either String (a, [MarcelToken])

data BTree a =  BNode a (BTree a) (BTree a) |
                BLeaf a |
                BEmpty
                deriving (Show, Eq)

data ExprTree = ETBNode Binop |
                ETBValue Value |
                ETBFunc FunctionCall |
                ETBBreak |
                ETBEmpty deriving (Show, Eq)

-- This function transform a full expression into a list of Expresions tokens
expression_as_list :: Expression -> [ExprTree]
expression_as_list (EValue v) = [ETBValue v]
expression_as_list (EFunction f) = [ETBFunc f]
expression_as_list (EBinop e1 b e2) = expression_as_list e1 ++
    [ETBNode b] ++ expression_as_list e2
expression_as_list (EBreak) = [ETBBreak]
expression_as_list x = error ("expression_as_list: " ++ show x)

-- Transforms a list of tokens into a expression tree for priority of operations
-- It works by splitting the list into two lists and a pivot (c.f. expression_list_split)
-- and then insert the expressions in the tree in the form
-- (left, pivot, right) where left and right are either nodes or leaves
-- nodes are operators and leaves are values
expression_list_as_tree :: [ExprTree] -> BTree ExprTree
expression_list_as_tree [] = BEmpty
expression_list_as_tree x =
    BNode root (expression_list_as_tree left) (expression_list_as_tree right)
    where
        (left, root, right) = expression_list_split x

-- This function comapres two expressions by their priority
-- Binop vs Binop: (Left == Right) || (Left < Right)
-- Binop vs Value: Right
-- Value vs Binop: Left
-- Value vs Value: Left
expression_tree_ord :: ExprTree -> ExprTree -> Bool
expression_tree_ord (ETBNode b1) (ETBNode b2)
    | b1 == b2 = True
    | otherwise = b1 < b2
expression_tree_ord (ETBValue _) (ETBNode _) = True
expression_tree_ord _ _ = False

-- This function finds the most significant operator in a list of expressions
-- The most significant operator is the one with the highest priority
-- If there are no operators, it returns 0
find_significant_index :: Int -> Int -> ExprTree -> [ExprTree] -> Int
find_significant_index _ i _ [] = i
find_significant_index count i sig (x:xs)
    | expression_tree_ord sig x =
        find_significant_index (count + 1) count x xs
    | otherwise = find_significant_index (count + 1) i sig xs

-- This function splits a list of expressions into two list and a pivot
-- The first list contains all the expression on the left of the most significant
-- operator, the second list contains all the expressions on the right of the
-- most significant operator
-- The most significant operator is the one with the highest priority
-- it is found by find_significant_index
expression_list_split :: [ExprTree] -> ([ExprTree], ExprTree, [ExprTree])
expression_list_split [] = ([], ETBEmpty, [])
expression_list_split [x] = ([], x, [])
expression_list_split x = (take best_index x,
    x !! best_index, drop (best_index + 1) x)
    where
        best_index = find_significant_index 0 0 (head x) x

-- This function splits a list of expressions into two lists
-- The first list contains all the expression on the left of the most significant
-- operator, the second list contains all the expressions on the right of the
-- most significant operator (which is the first argument)
-- if the list is empty, it returns two empty lists
expression_list_split' :: ExprTree -> [ExprTree] -> ([ExprTree], [ExprTree])
expression_list_split' _ [] = ([], [])
expression_list_split' x (y:ys)
    | expression_tree_ord x y = (y:left, right)
    | otherwise = (left, y:right)
    where
        (left, right) = expression_list_split' x ys

-- This function inserts an expression recursively into another expression
insert_expression :: Expression -> Binop -> Expression -> Expression
insert_expression (EValue v) b e = EBinop (EValue v) b e
insert_expression (EBinop e1 b1 e2) b2 e3 = 
    -- | b1 == b2 = EBinop e1 b1 (insert_expression e2 b2 e3)
    -- | b1 < b2 = EBinop e1 b1 (insert_expression e2 b2 e3)
    EBinop (insert_expression e1 b1 e2) b2 e3

-- This function parses a tree of tokens into an inorder expression
expression_tree_as_expression :: BTree ExprTree -> Expression
expression_tree_as_expression BEmpty = ENone
expression_tree_as_expression (BNode (ETBValue x) _ _) = EValue x
expression_tree_as_expression (BNode (ETBFunc x) _ _) = EFunction x
expression_tree_as_expression (BNode (ETBBreak) _ _) = EBreak
expression_tree_as_expression (BNode (ETBNode x) (BNode (ETBNode x') l' r') r) =
    insert_expression
    (expression_tree_as_expression r)
    x
    (expression_tree_as_expression (BNode (ETBNode x') l' r'))
expression_tree_as_expression (BNode (ETBNode x) l r) =
    insert_expression (expression_tree_as_expression l) x
    (expression_tree_as_expression r)

-- This function parses a list of tokens into a list of programs
-- It suceeds if every token can be parsed into a program
marcelAsAst :: [MarcelToken] -> MarcelAstResult [Program]
marcelAsAst [] = Right ([], [])
marcelAsAst x = marcelTokensAsProgram x >>=
    \x -> marcelAsAst (snd x) >>=
        \y -> Right (fst x:fst y, snd y)

-- This function parses a symbol from a list of tokens
-- It suceeds if the first token is a symbol
marcelTokenAsSingleSymbol :: [MarcelToken] -> MarcelAstResult String
marcelTokenAsSingleSymbol (TSymbol x:xs) = Right $ (x, xs)
marcelTokenAsSingleSymbol x = Left $ "Error: Expected symbol, got " ++ (show x)

-- This function generates a symbol from a list of tokens
-- It suceeds if the given token is the same as the first token in the list
marcelTokenExpect :: MarcelToken -> [MarcelToken] -> MarcelAstResult ()
marcelTokenExpect x (y:xs)
    | x == y = Right ((), xs)
    | otherwise = Left $ "Error: Expected " ++ (show x) ++ ", got " ++ (show y)
marcelTokenExpect x [] = Left $ "Error: Expected " ++ (show x) ++ ", got EOF"

-- This function generates a program from a list of tokens
-- It suceeds if it sucessfully parses in order
-- TFunc, function parser
marcelTokensAsProgram :: [MarcelToken] -> MarcelAstResult Program
marcelTokensAsProgram (TFunc:xs) = marcelTokensAsFunc xs >>=
    \x -> return (PFunction $ fst x, snd x)
marcelTokensAsProgram x = Left $ "Error: Expected function but got: " ++ show x

-- This function generates a function from a list of tokens
-- It suceeds if it sucessfully parses in order
-- TSymbol, TSymbol, TBinop BAssing, FuncArgs, FuncBody
marcelTokensAsFunc :: [MarcelToken] -> MarcelAstResult Function
marcelTokensAsFunc [] = Left "Error: Expected function name but is EOF"
marcelTokensAsFunc x = marcelTokenAsSingleSymbol x >>=
    \x -> marcelTokenAsSingleSymbol (snd x) >>=
    \y -> marcelTokenExpect (TBinop BAssing) (snd y) >>=
    \z -> marcelTokensAsFuncArgs (snd z) >>=
    \a -> marcelTokensAsFuncBody (snd a) >>=
    \b -> Right (Function (fst y) (fst x) (fst a) (fst b), snd b)

-- This function generates a list of symbols from a list of tokens
-- for the function arguments declaration
-- It suceeds if it sucessfully parses in order
-- TLeftBracket, [TSymbol, ...], TRightBracket
marcelTokensAsFuncArgs :: [MarcelToken] -> MarcelAstResult [String] 
marcelTokensAsFuncArgs [] = Left "Error: Expected function args but is EOF"
marcelTokensAsFuncArgs x = (marcelTokenExpect TLeftBracket x) >>=
    (\y -> marcelTokensAsFuncArgs' (snd y)) >>=
    (\z -> (marcelTokenExpect TRightBracket (snd z)) >>=
    (\w -> return (fst z, snd w)))

-- This function generates a list of Function arguments from a list of tokens
-- It never fails, it will never fail and stops when it does not encounter a
-- TSymbol token
marcelTokensAsFuncArgs' :: [MarcelToken] -> MarcelAstResult [String]
marcelTokensAsFuncArgs' (TSymbol x:xs) = (marcelTokensAsFuncArgs' xs) >>=
    (\x' -> return (x:fst x', snd x'))
marcelTokensAsFuncArgs' x = return ([], x)

-- This function generates a list of statements from a list of tokens
-- It will check for the TBegin token before calling marcelTokensAsFuncBody'
-- c.f marcelTokensAsFuncBody'
marcelTokensAsFuncBody :: [MarcelToken] -> MarcelAstResult [Statement]
marcelTokensAsFuncBody (TBegin:xs) = marcelTokensAsFuncBody' xs
marcelTokensAsFuncBody x = Left $
    "Error: Expected begin keyword for scope start but got: " ++ show x

-- This function generates a list of statements from a list of tokens
-- It will fail if the list of tokens does not end with the TEnd token
-- It is used by marcelTokensAsFuncBody which checks for the TBegin token
-- before calling this function
-- This function is also useful as it also used to parse if, while, elseif
-- scopes etc...
marcelTokensAsFuncBody' :: [MarcelToken] -> MarcelAstResult [Statement]
marcelTokensAsFuncBody' (TEnd:xs) = Right ([], xs)
marcelTokensAsFuncBody' [] = Left "Error: Expected end keyword but got EOF"
marcelTokensAsFuncBody' x = marcelTokensAsStatement x >>=
    \x -> marcelTokensAsFuncBody' (snd x) >>=
    \y -> Right (fst x:fst y, snd y)

-- Transform a Statement into an Expression
-- This function is considered unsafe because it will fail if the statement
-- is not an expression
_sexpr_as_expr :: Statement -> Expression
_sexpr_as_expr (SExpression e) = e

-- Transform a list of tokens into an statement expression
-- A statement expression is an expression followed by a semicolon
marcelTokensAsStatementExpr :: [MarcelToken] -> MarcelAstResult Statement
marcelTokensAsStatementExpr x =
    marcelTokensAsExpression x >>= \x ->
        if _head_is_binop (snd x) then
            marcelTokensAsStatementExpr (tail $ snd x) >>=
            \y -> return (SExpression $
                EBinop (fst x) (_as_binop $ head $ snd x)
                (_sexpr_as_expr $ fst y), snd y)
        else marcelTokenExpect TSemicolon (snd x) >>=
            \y -> return (SExpression (fst x), snd y)

-- Transform a list of tokens into a statement
-- A statement can be an if statement, a while statement, a return statement,
-- a declaration or an expression
marcelTokensAsStatement :: [MarcelToken] -> MarcelAstResult Statement
marcelTokensAsStatement (TIf:xs) = marcelTokensAsIfStatement xs
marcelTokensAsStatement (TWhile:xs) = marcelTokensAsWhileStatement xs
marcelTokensAsStatement (TFor:xs) = marcelTokensAsForStatement xs
marcelTokensAsStatement (TReturn:xs) = marcelTokensAsReturnStatement xs
marcelTokensAsStatement x = marcelTokensAsDeclaration x <|> 
    marcelTokensAsStatementExpr x

marcelTokensAsDeclaration :: [MarcelToken] -> MarcelAstResult Statement
marcelTokensAsDeclaration (TSymbol x:TSymbol y:TBinop BAssing:xs) =
    marcelTokensAsStatementExpr xs >>= \(expr, rest) ->
    return (SDeclaration (VarDeclaration y x (_sexpr_as_expr expr)), rest)
marcelTokensAsDeclaration x =
    Left $ "Error: Expected declaration but got: " ++ show x

-- Transform a list of tokens into an if statement
-- An if statement is formed by the keyword "if" followed by a condition,
-- a begin keyword, a list of statements and an end keyword
-- An if statement can also have an optional elseif statement and an optional
-- else statement
marcelTokensAsIfStatement :: [MarcelToken] -> MarcelAstResult Statement
marcelTokensAsIfStatement xs =
    marcelTokensAsExpressions xs >>=
    \(cond, rest) -> marcelTokensAsFuncBody rest >>=
    \(body, maybe_cond) -> (
        marcelTokenExpect TElseIf maybe_cond >>= \(_, xs) ->
        marcelTokensAsElseIfStatement xs >>= \(SElseIfStatement s, rest') ->
        return $ (SIfStatement $ IfStatement cond body (Just s) Nothing, rest')
    ) <|> (
        marcelTokenExpect TElse maybe_cond >>= \(_, xs) ->
        marcelTokensAsElseStatement xs >>= \(SElseStatement s, rest') ->
        return $ (SIfStatement $ IfStatement cond body Nothing (Just s), rest')
    ) <|> (return $ (SIfStatement $ IfStatement cond body Nothing Nothing, maybe_cond))


-- Transform a list of tokens into an elseif statement
-- An elseif statement is formed by the keyword "elseif" followed by a
-- condition, a begin keyword, a list of statements and an end keyword
-- The elseif statement can be followed by another elseif statement or an else statement
marcelTokensAsElseIfStatement :: [MarcelToken] -> MarcelAstResult Statement
marcelTokensAsElseIfStatement xs =
    marcelTokensAsExpressions xs >>= \(cond, rest) ->
    marcelTokensAsFuncBody rest >>= \(body, maybe_cond) -> (
        marcelTokenExpect TElseIf maybe_cond >>= \(_, xs) ->
        marcelTokensAsElseIfStatement xs >>= \(SElseIfStatement s, rest') ->
        return $ (SElseIfStatement $ ElseIfStatement cond body (Just s) Nothing, rest')
    ) <|> (
        marcelTokenExpect TElse maybe_cond >>= \(_, xs) ->
        marcelTokensAsElseStatement xs >>= \(SElseStatement s, rest') ->
        return $ (SElseIfStatement $ ElseIfStatement cond body Nothing (Just s), rest')
    ) <|> (return $ (SElseIfStatement $ ElseIfStatement cond body Nothing Nothing, maybe_cond))

-- Transform a list of tokens into an else statement
-- An else statement is formed by the keyword "else" followed by a
-- begin keyword, a list of statements and an end keyword
-- The begin and end are considered scope and can be parsed by the funcBody parser
marcelTokensAsElseStatement :: [MarcelToken] -> MarcelAstResult Statement
marcelTokensAsElseStatement xs = marcelTokensAsFuncBody xs >>= \x ->
    return $ (SElseStatement $ ElseStatement (fst x), snd x)

-- Transform a list of tokens into a while statement
-- A while statement is formed by the keyword "while" followed by an
-- expression, a begin keyword, a list of statements and an end keyword
-- The begin and end are considered scope and can be parsed by the funcBody parser
marcelTokensAsWhileStatement :: [MarcelToken] -> MarcelAstResult Statement
marcelTokensAsWhileStatement xs = marcelTokensAsExpressions xs >>=
    \x -> marcelTokensAsFuncBody (snd x) >>=
    \y -> return (SWhileStatement $ WhileStatement (fst x) (fst y), snd y)

marcelTokensAsForStatement :: [MarcelToken] -> MarcelAstResult Statement
marcelTokensAsForStatement xs = marcelTokensAsStatement xs >>=
    \x -> return x >>=
    \y -> marcelTokensAsExpressions (snd y) >>=
    \z -> marcelTokenExpect TSemicolon (snd z) >>=
    \w -> marcelTokensAsExpressions (snd w) >>=
    \v -> marcelTokensAsFuncBody (snd v) >>=
    \u -> return (SForStatement $
        ForStatement (fst x) (fst z) (fst v) (fst u), snd u)

-- Transform a list of tokens into a return statement
-- A return statement is formed by the keyword "return" followed by an
-- expression and a semicolon
marcelTokensAsReturnStatement :: [MarcelToken] -> MarcelAstResult Statement
marcelTokensAsReturnStatement xs = marcelTokensAsStatementExpr xs >>=
    \(SExpression e, rest) -> return (SReturn $ e, rest)

marcelTokensAsExpressions' :: [MarcelToken] -> MarcelAstResult [Expression]
marcelTokensAsExpressions' x = 
    (marcelTokensAsExpression x >>=
        \x' -> marcelTokensAsExpressions' (snd x') >>=
        \y -> return (fst x' : fst y, snd y)) <|>
    (Right ([], x)) 

-- Check if the first element of a list of tokens is a binary operator
-- This function is safe because it will return False if the list is empty
-- or if the first element is not a binary operator
_head_is_binop :: [MarcelToken] -> Bool
_head_is_binop (TBinop _:_) = True
_head_is_binop _ = False

-- Transform a list of tokens into a single expression
-- Can be considered unsafe because it only handles TBinop values
-- and will throw an error if it encounters a non-TBinop value
_as_binop :: MarcelToken -> Binop
_as_binop (TBinop x) = x

-- Error message for when we expect an expression but get noparse
marcelTokensAsExprNoParse :: [MarcelToken] -> MarcelAstResult [Expression]
marcelTokensAsExprNoParse x = Left $
    "Error: Expected expr but got noparse from " ++ show x

-- Error message for when we expect an expression after a binary operator
marcelTokensAsExprBinopError :: [MarcelToken] -> MarcelAstResult Expression
marcelTokensAsExprBinopError x = Left $
    "Error: Expected expression after binary operator but got: " ++ show x

-- Transform a list of tokens into a big expression
-- This function will parse a single expression and then
-- check if the next token is a binary operator. If it is,
-- it will parse another expression and then combine the two
-- expressions into a single expression. This will continue
-- until the next token is not a binary operator.
marcelTokensAsExpressions :: [MarcelToken] -> MarcelAstResult Expression
marcelTokensAsExpressions x
    | _head_is_binop rest = (marcelTokensAsExpressions (tail rest) >>= \x ->
        return (EBinop result (_as_binop $ head rest) (fst x), snd x)) <|>
        (marcelTokensAsExprBinopError $ tail rest)
    | otherwise = Right $ (result, rest)
    where
        Right (result, rest) = marcelTokensAsExpression x

-- Transform a list of tokens into an expression
-- Expression can either be a value, a function call, a binary operation
-- or a break statement
marcelTokensAsExpression :: [MarcelToken] -> MarcelAstResult Expression
marcelTokensAsExpression (TSymbol n:TLeftParen:xs) = marcelTokensAsFCall n xs
marcelTokensAsExpression (TSymbol n:xs) = Right $ (EValue $ VSymbol n, xs)
marcelTokensAsExpression (TChar n:xs)   = Right $ (EValue $ VChar n, xs)
marcelTokensAsExpression (TNumber n:TDot:TNumber m:xs) =
    Right $ (EValue $ VFloating $ read $ show n ++ "." ++ show m, xs)
marcelTokensAsExpression (TNumber n:xs) = Right $ (EValue $ VNumber n, xs)
marcelTokensAsExpression (TString n:xs) = Right $ (EValue $ VString n, xs)
marcelTokensAsExpression (TBool n:xs)   = Right $ (EValue $ VBool n, xs)
marcelTokensAsExpression (TBreak:xs) = Right $ (EBreak, xs)
marcelTokensAsExpression (TLeftParen:xs) = marcelTokensAsExpressions xs >>=
    \x -> marcelTokenExpect TRightParen (snd x) >>=
    \y -> return (EValue $ VParen $ fst x, snd y)
marcelTokensAsExpression x = Left $ "Expected Expression but got: " ++ show x

-- Transform a list of tokens as a function call
-- The first token must be a symbol
-- The second token must be a left parenthesis
-- The last token must be a right parenthesis
marcelTokensAsFCall :: String -> [MarcelToken] -> MarcelAstResult Expression
marcelTokensAsFCall n x = marcelTokensAsFCallArgs x >>=
    \x' -> marcelTokenExpect TRightParen (snd x') >>=
    \y -> return (EFunction $ FunctionCall n (fst x'), snd y)

-- Error message for unexpected comma in function call
marcelAsFCallUnexpectedComma :: [MarcelToken] -> MarcelAstResult [Expression]
marcelAsFCallUnexpectedComma x = Left $
     "Expected expression after ',' in arguments for fcall but got: "
     ++ show x

-- Transform a list of tokens as a list of expressions
-- Thoses expressions are considered to be arguments for a function call
-- Arguments are separated by commas
marcelTokensAsFCallArgs :: [MarcelToken] -> MarcelAstResult [Expression]
marcelTokensAsFCallArgs (TRightParen:xs) = Right ([], TRightParen:xs)
marcelTokensAsFCallArgs x = marcelTokensAsExpressions x >>=
    \x' -> (marcelTokenExpect TComma (snd x') >>=
        \xs -> marcelTokensAsFCallArgs (snd xs) >>=
        \x'' -> return ((fst x'):(fst x''), snd x'')) <|>
        (marcelTokenExpect TRightParen (snd x') >>=
            \xs -> return ([fst x'], snd x')) <|>
        (marcelAsFCallUnexpectedComma $ snd x')

{-
-- ensure that the expression contained is following the priority rules
reorder_programs :: [Program] -> [Program]
reorder_programs [] = []
reorder_programs (PFunction f:xs) =
    (PFunction (reorder_function f)):reorder_programs xs

-- ensure that the expression contained is following the priority rules
reorder_function :: Function -> Function
reorder_function (Function n ret args body) =
    Function n ret args (reorder_statements body)

-- ensure that the expression contained is following the priority rules
reorder_expression :: Expression -> Expression
reorder_expression e = expression_tree_as_expression $
    expression_list_as_tree $ expression_as_list e

-- ensure that the expression contained is following the priority rules
reorder_statement :: Statement -> Statement
reorder_statement (SIfStatement if') = SIfStatement $ reorder_if if'
reorder_statement (SWhileStatement while') =
    SWhileStatement $ reorder_while while'
reorder_statement (SDeclaration decl) = SDeclaration $ reorder_declaration decl
reorder_statement (SExpression e) = SExpression $ reorder_expression e
reorder_statement (SReturn e) = SReturn $ reorder_expression e

-- ensure that the expression contained is following the priority rules
reorder_if :: IfStatement -> IfStatement
reorder_if (IfStatement e body else_if else') =
    IfStatement (reorder_expression e)
    (reorder_statements body) (reorder_else_if else_if)
    (reorder_else else')

-- ensure that the expression contained is following the priority rules
reorder_else_if :: Maybe ElseIfStatement -> Maybe ElseIfStatement
reorder_else_if Nothing = Nothing
reorder_else_if (Just (ElseIfStatement e body else_if else')) =
    Just $ ElseIfStatement (reorder_expression e)
    (reorder_statements body) (reorder_else_if else_if)
    (reorder_else else')

-- ensure that the expression contained is following the priority rules
reorder_else :: Maybe ElseStatement -> Maybe ElseStatement
reorder_else Nothing = Nothing
reorder_else (Just (ElseStatement body)) =
    Just $ ElseStatement $ reorder_statements body

-- ensure that the expression contained is following the priority rules
reorder_while :: WhileStatement -> WhileStatement
reorder_while (WhileStatement e body) =
    WhileStatement (reorder_expression e)
    (reorder_statements body)

-- ensure that the expression contained is following the priority rules
reorder_declaration :: VarDeclaration -> VarDeclaration
reorder_declaration (VarDeclaration n t e) =
    VarDeclaration n t (reorder_expression e)

-- ensure that the expression is following the priority rules
-- 5 + 4 * 3 -> (5 + (4 * 3)) -> 4 * 3 + 5
reorder_statements :: [Statement] -> [Statement]
reorder_statements x = map reorder_statement x
-}

marcelFromFile' :: String -> MarcelAstResult [Program]
marcelFromFile' content = case tokenizeMarcel content of
    Right x -> (return $ (transformMarcelTokens x, [])) >>=
        \y -> (marcelAsAst $ fst y) >>=
        \z -> return (fst z, [])
    Left (l, r) -> Left $ "(" ++ l ++ ", " ++ r ++ ")"

-- Does the Full pipeline of parsing
-- It takes a filename and returns a list of programs
-- A list of program is either a function or a variable declaration
marcelFromFile :: String -> IO (MarcelAstResult [Program])
marcelFromFile filename = openFile filename ReadMode >>=
    \handle -> hGetContents handle >>= \contents -> 
        return $ marcelFromFile' contents
