module MarcelVMDef (
    VMVar(..),
    VMStack,
    VM(..)
    , base_vm
    , push_stack
    , push_stack'
    , pop_break
    , push_break
    , pop_stack
    , push_var
    , pop_var
    , get_var
    , check_type'
    , set_accu
    , get_accu
    , get_accu'
    , set_var
    , value_is_null
    , declare_var
) where

{-# OPTIONS_GHC -WEverything #-}

import MarcelDef (Value(..), Function(..))

data VMVar = VMVar {
    _vmvar_name :: String,
    _vmvar_type :: String,
    _vmvar_value :: Value
} deriving (Show)

type VMStack = [VMVar]

data VM = VM {
    _vm_stack :: [VMStack],
    _vm_accu :: Value,
    _returning :: [Bool],
    _break :: [Bool],
    _vm_funcs :: [Function],
    _vm_buffer :: String
} deriving (Show)


base_vm :: VM
base_vm = VM [[]] (VNumber 0) [False] [] [] ""

push_stack :: VM -> VM
push_stack vm = vm { _vm_stack = [] : _vm_stack vm,
    _returning = False : _returning vm }

push_stack' :: VM -> VMStack -> VM
push_stack' vm stack = vm { _vm_stack = stack : _vm_stack vm,
    _returning = False : _returning vm }

pop_break :: VM -> VM
pop_break vm = vm { _break = tail $ _break vm }

push_break :: VM -> VM
push_break vm = vm { _break = False : _break vm }

pop_stack :: VM -> VM
pop_stack vm = vm { _vm_stack = tail $ _vm_stack vm,
    _returning = tail $ _returning vm }

push_var :: VM -> VMVar -> VM
push_var vm var = vm {
    _vm_stack = ((var :
        filter (\x -> _vmvar_name x /= _vmvar_name var)
            (head (_vm_stack vm))) :
        tail (_vm_stack vm)) }

pop_var :: VM -> VM
pop_var vm = vm { _vm_stack = tail (head (_vm_stack vm)) :
    tail (_vm_stack vm) }

get_var :: VM -> String -> Either String VMVar
get_var vm name = case res of
    (x:_) -> Right x
    [] -> Left $ "Error: Variable " ++ name ++ " not found"
    where res = filter (\x -> _vmvar_name x == name) (head (_vm_stack vm))

check_type' :: Value -> String -> Either String Value
check_type' (VNumber v) "int" = Right $ VNumber v
check_type' (VFloating v) "float" = Right $ VFloating v
check_type' (VNumber v) "float" = Right $ VFloating $ fromIntegral v
check_type' (VFloating v) "int" = Right $ VNumber $ round v
check_type' (VString v) "string" = Right $ VString v
check_type' (VChar v) "char" = Right $ VChar v
check_type' (VChar v) "string" = Right $ VString [v]
check_type' (VBool v) "bool" = Right $ VBool v
check_type' (VBool v) t = check_type' (VNumber $ if v then 1 else 0) t
check_type' v "any" = Right v
check_type' v t = Left $ "Error: Type mismatch: " ++ show v ++ " is not a " ++ t

set_var' :: VMStack -> String -> Value -> Either String VMStack
set_var' [] name value = Left $ "Error: Variable " ++ name ++ " not found"
set_var' (x:xs) name value
    | _vmvar_name x == name =
        check_type' value (_vmvar_type x) >>= \value' ->
        Right $ (x { _vmvar_value = value' }) : xs
    | otherwise = set_var' xs name value >>=
                \rest -> return $ x:rest

set_var :: VM -> String -> Value -> Either String VM
set_var vm name value = set_var' (head (_vm_stack vm)) name value >>=
    \x -> Right $ vm { _vm_stack = x : tail (_vm_stack vm) }

set_accu :: VM -> Value -> VM
set_accu vm value = vm { _vm_accu = value }

get_accu :: VM -> Value
get_accu vm = _vm_accu vm

get_accu' :: VM -> Either a Value
get_accu' vm = Right $ _vm_accu vm

value_is_null :: Value -> Bool
value_is_null (VNumber 0) = True
value_is_null (VFloating 0) = True
value_is_null (VString "") = True
value_is_null (VChar '\0') = True
value_is_null (VSymbol "") = True
value_is_null (VBool False) = True
value_is_null _ = False

declare_var :: VM -> String -> String -> Either String VM
declare_var vm name "int" = Right $ push_var vm $ VMVar name "int" (VNumber 0)
declare_var vm name "float" = Right $ push_var vm $ VMVar name "float" (VFloating 0)
declare_var vm name "string" = Right $ push_var vm $ VMVar name "string" (VString "")
declare_var vm name "char" = Right $ push_var vm $ VMVar name "char" (VChar '\0')
declare_var vm name "bool" = Right $ push_var vm $ VMVar name "bool" (VBool False)
declare_var vm name "any" = Right $ push_var vm $ VMVar name "any" (VNumber 0)
declare_var vm name x = Left $ "Error: Type " ++ x ++ " is invalid"
