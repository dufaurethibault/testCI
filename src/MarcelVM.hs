module MarcelVM (
    run_program
) where

{-# OPTIONS_GHC -WEverything #-}

-- import Debug.Trace

import MarcelDef (
    Value(..)
    , Function(..)
    , Program(..)
    , FunctionCall(..)
    , Expression(..)
    , ForStatement(..)
    , WhileStatement(..)
    , Statement(..)
    , IfStatement(..)
    , ElseIfStatement(..)
    , ElseStatement(..)
    , VarDeclaration(..)
    , Binop(..)
    )

import MarcelAst (marcelFromFile)
import MarcelOperations (run_binop)
import MarcelVMDef (
    VM(..)
    , VMStack(..)
    , VMVar(..)
    , base_vm
    , push_stack
    , push_stack'
    , get_accu
    , get_accu'
    , pop_break
    , push_break
    , value_is_null
    , set_accu
    , set_var
    , declare_var
    , get_var
    , check_type'
    , pop_stack
    )

run :: Function -> VM -> Either String VM
run (Function name ret _ statements) vm =
    run_statements statements vm >>=
    \x -> if _must_break x then
        Left "Error: Break outside of loop"
        else case check_type' (get_accu x) ret of
            Left x -> Left $
                "Invalid return type in function " ++ name ++ ": " ++ x
            _ -> (return $ pop_stack x)

run_io :: Function -> VM -> IO (Either String VM)
run_io (Function "main" "int" [] statements) vm = do
    res <- run_statements_io statements vm
    case res of
        Left x -> return $ Left x
        Right x -> if _must_break x then
            return $ Left "Error: Break outside of loop"
        else case check_type' (get_accu x) "int" of
            Left x -> return $ Left $
                "Invalid return type in main: " ++ x
            Right _ -> return $ Right $ pop_stack x
run_io (Function _ _ _ _) _ = return $
    Left "Main function must be declared as function int main = []"


_must_break :: VM -> Bool
_must_break (VM _ _ _ [] _ _) = False
_must_break (VM _ _ _ (x:xs) _ _) = x

_stopping_statements :: VM -> Bool
_stopping_statements vm = (head $ _returning vm) ||
    _must_break vm

run_statements_io :: [Statement] -> VM -> IO (Either String VM)
run_statements_io [] vm = return $ Right vm
run_statements_io (x:xs) vm = case run_statement x vm of
    Left x -> return $ Left x
    Right vm' ->
        if _stopping_statements vm' then
            (putStr $ _vm_buffer vm') >>
                (return $ Right vm' { _vm_buffer = "" })
        else 
            (putStr $ _vm_buffer vm') >>
                run_statements_io xs vm' { _vm_buffer = ""}

run_statements :: [Statement] -> VM -> Either String VM
run_statements [] vm = Right vm
run_statements (x:xs) vm =
    run_statement x vm >>= \vm' ->
    if _stopping_statements vm' then
        --trace ("Statement: " ++ show vm')
        return vm'
    else run_statements xs vm'

run_statement' :: Statement -> VM -> Either String VM
run_statement' (SIfStatement (IfStatement cond iftrue eif else')) vm =
    run_statement_if cond iftrue vm eif else'
run_statement' (SWhileStatement while) vm = run_statement_while while vm
run_statement' (SForStatement for) vm = run_statement_for for vm
run_statement' (SReturn (e)) vm = run_statement_return e vm
run_statement' (SDeclaration (VarDeclaration name type' expr)) vm =
    run_statement_declaration name type' expr vm
run_statement' (SExpression expr) vm = run_statement_expression expr vm

run_statement :: Statement -> VM -> Either String VM
run_statement x vm = --trace (show x ++ " -> " ++ show vm)
    run_statement' x vm

run_statement_if :: Expression -> [Statement] -> VM -> Maybe ElseIfStatement -> Maybe ElseStatement -> Either String VM
run_statement_if cond iftrue vm elseif else' =
    run_expression cond vm >>= \vm ->
        if value_is_null $ get_accu vm then
            case elseif of
                Just (ElseIfStatement cond' iftrue' elseif else') ->
                    run_statement_elseif cond' iftrue' vm elseif else'
                Nothing -> case else' of
                    Just (ElseStatement iftrue') -> run_statement_else iftrue' vm
                    Nothing -> return $ vm
        else
            run_statements iftrue vm

run_statement_elseif :: Expression -> [Statement] -> VM -> Maybe ElseIfStatement -> Maybe ElseStatement -> Either String VM
run_statement_elseif cond iftrue vm elseif else' = 
    run_expression cond vm >>= \vm ->
        if value_is_null $ get_accu vm then
            case elseif of
                Just (ElseIfStatement cond' iftrue' elseif else') ->
                    run_statement_elseif cond' iftrue' vm elseif else'
                Nothing -> case else' of
                    Just (ElseStatement iftrue') -> run_statement_else iftrue' vm
                    Nothing -> return $ vm
        else run_statements iftrue vm

run_statement_else :: [Statement] -> VM -> Either String VM
run_statement_else iftrue vm = run_statements iftrue vm

run_statement_declaration :: String -> String -> Expression -> VM -> Either String VM
run_statement_declaration name type' e vm =
    declare_var vm name type' >>= \vm -> case e of
        ENone -> return $ vm
        _ -> run_expression (EBinop (EValue (VSymbol name)) BAssing e) vm

run_statement_expression :: Expression -> VM -> Either String VM
run_statement_expression expr vm = run_expression expr vm

run_statement_return :: Expression -> VM -> Either String VM
run_statement_return expr vm = run_expression expr vm
    >>= \vm -> return $ vm { _returning = True : tail (_returning vm) }

run_expression :: Expression -> VM -> Either String VM
run_expression (EValue (VSymbol x)) vm =
    get_var vm x >>=
        \var -> return $ set_accu vm $ _vmvar_value var 
run_expression (EValue (VParen x)) vm = run_expression x vm
run_expression (EValue x) vm = return $ set_accu vm x
run_expression (EBinop (EValue (VSymbol l)) BAssing e2) vm =
    run_expression e2 vm >>= \vm -> set_var vm l $ get_accu vm
run_expression (EBinop e1 op e2) vm = run_expression' (EBinop e1 op e2) vm
run_expression (EBreak) vm = return $
    vm { _break = True:tail (_break vm) }
run_expression (EFunction e) vm = run_function_call vm e

run_expression' :: Expression -> VM -> Either String VM
run_expression' (EBinop e1 op e2) vm =
    run_expression e1 vm >>=
    \vm -> get_accu' vm >>= \v1 ->
    run_expression e2 vm >>=
    \vm -> get_accu' vm >>= \v2 ->
    run_binop v1 op v2 vm

run_statement_while :: WhileStatement -> VM -> Either String VM
run_statement_while (WhileStatement cond iftrue) x =
    run_expression cond (push_break x) >>= \vm ->
    if (value_is_null $ get_accu vm) then
        return $ pop_break vm
    else run_statements iftrue vm >>= (\vm ->
        if (_must_break vm) then return $ pop_break vm
        else run_statement_while (WhileStatement cond iftrue) vm)

run_statement_for :: ForStatement -> VM -> Either String VM
run_statement_for (ForStatement init cond end iftrue) x =
    run_statement init x >>= \vm ->
    run_statement_for' (ForStatement init cond end iftrue) vm

run_statement_for' :: ForStatement -> VM -> Either String VM
run_statement_for' (ForStatement _ cond end iftrue) x =
    run_expression cond (push_break x) >>= \vm ->
    if (value_is_null $ get_accu vm) then
        return $ pop_break vm
    else run_statements iftrue vm >>= (\vm ->
        if (_must_break vm) then return $ pop_break vm
        else run_expression end vm >>= \vm ->
            run_statement_for' (ForStatement (SExpression ENone) cond end iftrue) vm)

vm_get_function :: VM -> String -> Either String Function
vm_get_function vm name = case res of
    [] -> Left $ "Error: Function " ++ name ++ " not found"
    (x:_) -> Right $ x
    where res = filter (\x -> fname x == name) $ _vm_funcs vm

generate_stack_frame :: VM -> ([String], [Expression]) -> Either String VMStack
generate_stack_frame vm ([], []) = Right $ []
generate_stack_frame vm ([], _) = Left $ "Error: Too many arguments"
generate_stack_frame vm (_, []) = Left $ "Error: Too few arguments"
generate_stack_frame vm (name:type':names, expr:exprs) =
    run_expression expr vm
        >>= \vm' -> get_accu' vm'
        >>= \value -> generate_stack_frame vm' (names, exprs)
        >>= \stack -> return $ ((VMVar type' name value):stack)
        >>= \stack -> return $ stack

generate_anonymous_stack_frame' :: VM -> [Expression] -> Int -> Either String VMStack
generate_anonymous_stack_frame' vm [] _ = Right $ []
generate_anonymous_stack_frame' vm (expr:exprs) i =
    run_expression expr vm
        >>= \vm' -> get_accu' vm'
        >>= \value -> generate_anonymous_stack_frame' vm' exprs (i+1)
        >>= \stack -> return $ ((VMVar "auto" ("arg" ++ show i) value):stack)

generate_anonymous_stack_frame :: VM -> [Expression] -> Either String VMStack
generate_anonymous_stack_frame vm exprs = generate_anonymous_stack_frame' vm exprs 0

run_function_call :: VM -> FunctionCall -> Either String VM
run_function_call vm (FunctionCall "print" args) =
    generate_anonymous_stack_frame vm (args) >>=
        \x -> (return $ vm { _vm_buffer = _vm_buffer vm ++
            (concat $ map print_arg $ map _vmvar_value x) })
    
run_function_call vm (FunctionCall name args) = 
    vm_get_function vm name >>= \func ->
    generate_stack_frame vm (fargs func, args) >>= \x ->
    run func $ (push_stack' vm x)

print_arg :: Value -> String
print_arg (VString x) = x
print_arg (VChar x) = [x]
print_arg (VNumber x) = show x
print_arg (VFloating x) = show x
print_arg (VBool x) = show x
print_arg (VSymbol x) = show x

get_return :: VM -> Int
get_return (VM _ (VNumber x) _ _ _ _) = x
get_return (VM _ (VFloating x) _ _ _ _) = round x
get_return (VM _ (VBool x) _ _ _ _) = if x then 1 else 0

run_main :: VM -> [Function] -> IO (Int)
run_main vm funcs = case filter (\f -> fname f == "main") funcs of
    [] -> putStrLn "Error: No main function" >>
        return 1
    [f] -> do
        res <- run_io f (vm { _vm_funcs = funcs })
        case res of
            Left x -> do
                putStrLn $ x
                return 1
            Right x -> return $ get_return x
    _ -> do
        putStrLn "Error: Multiple main functions"
        return 1

extract_functions_from_program :: [Program] -> [Function]
extract_functions_from_program ((PFunction x):xs) =
    x:extract_functions_from_program xs
extract_functions_from_program (_:xs) = extract_functions_from_program xs
extract_functions_from_program _ = []

run_program :: String -> IO (Int)
run_program file = marcelFromFile file >>= \x -> case x of
    Left x -> do
        putStrLn $ "Error: " ++ x
        return 2
    Right x -> run_main (base_vm) (extract_functions_from_program $ fst x)
