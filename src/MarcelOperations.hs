module MarcelOperations (
    run_binop
) where

{-# OPTIONS_GHC -WEverything #-}

import MarcelVMDef (
    VM(..),
    VMVar(..),
    VMStack,
    get_var,
    set_var,
    set_accu,
    value_is_null,
    get_accu
    )

import MarcelDef (Value(..), Binop(..))

run_binop :: Value -> Binop -> Value -> VM -> Either String VM
run_binop (VNumber l) b vr vm = run_binop_int l b vr vm
run_binop (VFloating l) b vr vm = run_binop_float l b vr vm
run_binop (VString l) b vr vm = run_binop_string l b vr vm
run_binop (VChar l) b vr vm = run_binop_char l b vr vm
run_binop (VSymbol l) b vr vm = run_binop_symbol l b vr vm
run_binop (VBool l) b vr vm = run_binop_bool l b vr vm
run_binop l b r _ = Left $ "Error: Cannot do binary operation on "
    ++ (show l) ++ " and " ++ (show r)
    ++ " with operator " ++ (show b)

run_binop_int :: Int -> Binop -> Value -> VM -> Either String VM
run_binop_int l b (VNumber r) vm = run_binop_int_int l b r vm
run_binop_int l b (VFloating r) vm = run_binop_int_float l b r vm
run_binop_int l b (VString r) vm = run_binop_int_string l b r vm
run_binop_int l b (VChar r) vm = run_binop_int_char l b r vm
run_binop_int l b (VSymbol r) vm = run_binop_int_symbol l b r vm
run_binop_int l b (VBool r) vm = run_binop_int_bool l b r vm

run_binop_int_int :: Int -> Binop -> Int -> VM -> Either String VM
run_binop_int_int l BPlus r vm = return $ set_accu vm $ VNumber $ l + r
run_binop_int_int l BMinus r vm = return $ set_accu vm $ VNumber $ l - r
run_binop_int_int l BMul r vm = return $ set_accu vm $ VNumber $ l * r
run_binop_int_int l BDivide r vm = return $ set_accu vm $ VNumber $ l `div` r
run_binop_int_int l BModulo r vm = return $ set_accu vm $ VNumber $ l `mod` r
run_binop_int_int l BEquals r vm = return $ set_accu vm $ VBool $ l == r
run_binop_int_int l BNotEquals r vm = return $ set_accu vm $ VBool $ l /= r
run_binop_int_int l BLessThan r vm = return $ set_accu vm $ VBool $ l < r
run_binop_int_int l BGreaterThan r vm = return $ set_accu vm $ VBool $ l > r
run_binop_int_int l BLessThanEquals r vm = return $ set_accu vm $ VBool $ l <= r
run_binop_int_int l BGreaterThanEquals r vm = return $ set_accu vm $ VBool $ l >= r
run_binop_int_int l BAssing r vm = Left $ "Error: Cannot do int = int"

_accu_as_int :: VM -> Either String VM
_accu_as_int vm = case get_accu vm of
    VNumber x -> Right $ set_accu vm $ VNumber x
    VFloating x -> Right $ set_accu vm $ VNumber $ round x
    _ -> Left $ "Error: Accu is not an int"

_bool_to_int :: Bool -> Int
_bool_to_int True = 1
_bool_to_int False = 0

_bool_to_float :: Bool -> Float
_bool_to_float True = 1.0
_bool_to_float False = 0.0

_accu_as_bool :: VM -> Either String VM
_accu_as_bool vm =
    return $ set_accu vm $ VBool $
        not $ value_is_null $ get_accu vm

run_binop_int_float :: Int -> Binop -> Float -> VM -> Either String VM
run_binop_int_float l b r vm = run_binop_float_float (fromIntegral l) b r vm
    >>= _accu_as_int

run_binop_int_string :: Int -> Binop -> String -> VM -> Either String VM
run_binop_int_string l BPlus r vm = run_binop_string_string (show l) BPlus r vm
run_binop_int_string _ BMinus _ _ = Left $ "Error: Cannot do int - string"
run_binop_int_string l BMul r vm = run_binop_string_int r BMul l vm
run_binop_int_string _ BDivide _ _ = Left $ "Error: Cannot do int / string"
run_binop_int_string _ BModulo _ _ = Left $ "Error: Cannot do int % string"
run_binop_int_string _ BEquals _ _ = Left $ "Error: Cannot do int == string"
run_binop_int_string _ BNotEquals _ _ = Left $ "Error: Cannot do int != string"
run_binop_int_string _ BLessThan _ _ = Left $ "Error: Cannot do int < string"
run_binop_int_string _ BGreaterThan _ _ = Left $ "Error: Cannot do int > string"
run_binop_int_string _ BLessThanEquals _ _ = Left $ "Error: Cannot do int <= string"
run_binop_int_string _ BGreaterThanEquals _ _ = Left $ "Error: Cannot do int >= string"
run_binop_int_string _ BAssing _ _ = Left $ "Error: Cannot do int = string"

run_binop_int_char :: Int -> Binop -> Char -> VM -> Either String VM
run_binop_int_char l BPlus r vm = run_binop_int_string l BPlus [r] vm
run_binop_int_char _ BMinus _ _ = Left $ "Error: Cannot do int - char"
run_binop_int_char l BMul r vm = run_binop_int_string l BMul [r] vm
run_binop_int_char _ BDivide _ _ = Left $ "Error: Cannot do int / char"
run_binop_int_char _ BModulo _ _ = Left $ "Error: Cannot do int % char"
run_binop_int_char _ BEquals _ _ = Left $ "Error: Cannot do int == char"
run_binop_int_char _ BNotEquals _ _ = Left $ "Error: Cannot do int != char"
run_binop_int_char _ BLessThan _ _ = Left $ "Error: Cannot do int < char"
run_binop_int_char _ BGreaterThan _ _ = Left $ "Error: Cannot do int > char"
run_binop_int_char _ BLessThanEquals _ _ = Left $ "Error: Cannot do int <= char"
run_binop_int_char _ BGreaterThanEquals _ _ = Left $ "Error: Cannot do int >= char"
run_binop_int_char _ BAssing _ _ = Left $ "Error: Cannot do int = char"

run_binop_int_symbol :: Int -> Binop -> String -> VM -> Either String VM
run_binop_int_symbol l b r vm = get_var vm r >>=
    \v -> run_binop (VNumber l) b (_vmvar_value v) vm

run_binop_int_bool :: Int -> Binop -> Bool -> VM -> Either String VM
run_binop_int_bool l b r vm = run_binop_int_int l b (_bool_to_int r) vm

run_binop_float :: Float -> Binop -> Value -> VM -> Either String VM
run_binop_float l b (VNumber r) vm = run_binop_float_int l b r vm
run_binop_float l b (VFloating r) vm = run_binop_float_float l b r vm
run_binop_float l b (VString r) vm = run_binop_float_string l b r vm
run_binop_float l b (VChar r) vm = run_binop_float_char l b r vm
run_binop_float l b (VSymbol r) vm = run_binop_float_symbol l b r vm
run_binop_float l b (VBool r) vm = run_binop_float_bool l b r vm

run_binop_float_string :: Float -> Binop -> String -> VM -> Either String VM
run_binop_float_string l BPlus r vm = run_binop_string_string (show l) BPlus r vm
run_binop_float_string _ BMinus _ _ = Left $ "Error: Cannot do float - string"
run_binop_float_string _ BMul _ _ = Left $ "Error: Cannot do float * string"
run_binop_float_string _ BDivide _ _ = Left $ "Error: Cannot do float / string"
run_binop_float_string _ BModulo _ _ = Left $ "Error: Cannot do float % string"
run_binop_float_string _ BEquals _ _ = Left $ "Error: Cannot do float == string"
run_binop_float_string _ BNotEquals _ _ = Left $ "Error: Cannot do float != string"
run_binop_float_string _ BLessThan _ _ = Left $ "Error: Cannot do float < string"
run_binop_float_string _ BGreaterThan _ _ = Left $ "Error: Cannot do float > string"
run_binop_float_string _ BLessThanEquals _ _ = Left $ "Error: Cannot do float <= string"
run_binop_float_string _ BGreaterThanEquals _ _ = Left $ "Error: Cannot do float >= string"
run_binop_float_string _ BAssing _ _ = Left $ "Error: Cannot do float = string"

run_binop_float_int :: Float -> Binop -> Int -> VM -> Either String VM
run_binop_float_int l b r vm = run_binop_float_float l b (fromIntegral r) vm

run_binop_float_float :: Float -> Binop -> Float -> VM -> Either String VM
run_binop_float_float l BPlus r vm = return $ set_accu vm $ VFloating $ l + r
run_binop_float_float l BMinus r vm = return $ set_accu vm $ VFloating $ l - r
run_binop_float_float l BMul r vm = return $ set_accu vm $ VFloating $ l * r
run_binop_float_float l BDivide r vm = return $ set_accu vm $ VFloating $ l / r
run_binop_float_float _ BModulo _ _ = Left $ "Error: Cannot do float % float"
run_binop_float_float l BEquals r vm = return $ set_accu vm $ VBool $ l == r
run_binop_float_float l BNotEquals r vm = return $ set_accu vm $ VBool $ l /= r
run_binop_float_float l BLessThan r vm = return $ set_accu vm $ VBool $ l < r
run_binop_float_float l BGreaterThan r vm = return $ set_accu vm $ VBool $ l > r
run_binop_float_float l BLessThanEquals r vm = return $ set_accu vm $ VBool $ l <= r
run_binop_float_float l BGreaterThanEquals r vm = return $ set_accu vm $ VBool $ l >= r
run_binop_float_float _ BAssing _ _ = Left $ "Error: Cannot do float = float"

run_binop_float_char :: Float -> Binop -> Char -> VM -> Either String VM
run_binop_float_char l BPlus r vm = run_binop_string_char (show l) BPlus r vm
run_binop_float_char _ BMinus _ _ = Left $ "Error: Cannot do float - char"
run_binop_float_char _ BMul _ _ = Left $ "Error: Cannot do float * char"
run_binop_float_char _ BDivide _ _ = Left $ "Error: Cannot do float / char"
run_binop_float_char _ BModulo _ _ = Left $ "Error: Cannot do float % char"
run_binop_float_char _ BEquals _ _ = Left $ "Error: Cannot do float == char"
run_binop_float_char _ BNotEquals _ _ = Left $ "Error: Cannot do float != char"
run_binop_float_char _ BLessThan _ _ = Left $ "Error: Cannot do float < char"
run_binop_float_char _ BGreaterThan _ _ = Left $ "Error: Cannot do float > char"
run_binop_float_char _ BLessThanEquals _ _ = Left $ "Error: Cannot do float <= char"
run_binop_float_char _ BGreaterThanEquals _ _ = Left $ "Error: Cannot do float >= char"
run_binop_float_char _ BAssing _ _ = Left $ "Error: Cannot do float = char"

run_binop_float_symbol :: Float -> Binop -> String -> VM -> Either String VM
run_binop_float_symbol l b r vm = get_var vm r >>=
    \v -> run_binop (VFloating l) b (_vmvar_value v) vm

run_binop_float_bool :: Float -> Binop -> Bool -> VM -> Either String VM
run_binop_float_bool l b r vm = run_binop_float_int l b (_bool_to_int r) vm

run_binop_string :: String -> Binop -> Value -> VM -> Either String VM
run_binop_string l b (VNumber r) vm = run_binop_string_int l b r vm
run_binop_string l b (VFloating r) vm = run_binop_string_float l b r vm
run_binop_string l b (VString r) vm = run_binop_string_string l b r vm
run_binop_string l b (VChar r) vm = run_binop_string_char l b r vm
run_binop_string l b (VSymbol r) vm = run_binop_string_symbol l b r vm
run_binop_string l b (VBool r) vm = run_binop_string_bool l b r vm

run_binop_string_int :: String -> Binop -> Int -> VM -> Either String VM
run_binop_string_int l BPlus r vm = return $ set_accu vm $ VString $ l ++ show r
run_binop_string_int _ BMinus _ _ = Left $ "Error: Cannot do string - int"
run_binop_string_int l BMul r vm = return $ set_accu vm $ VString $ concat $ replicate r l
run_binop_string_int _ BDivide _ _ = Left $ "Error: Cannot do string / int"
run_binop_string_int _ BModulo _ _ = Left $ "Error: Cannot do string % int"
run_binop_string_int _ BEquals _ _ = Left $ "Error: Cannot do string == int"
run_binop_string_int _ BNotEquals _ _ = Left $ "Error: Cannot do string != int"
run_binop_string_int _ BLessThan _ _ = Left $ "Error: Cannot do string < int"
run_binop_string_int _ BGreaterThan _ _ = Left $ "Error: Cannot do string > int"
run_binop_string_int _ BLessThanEquals _ _ = Left $ "Error: Cannot do string <= int"
run_binop_string_int _ BGreaterThanEquals _ _ = Left $ "Error: Cannot do string >= int"
run_binop_string_int _ BAssing _ _ = Left $ "Error: Cannot do string = int"

run_binop_string_float :: String -> Binop -> Float -> VM -> Either String VM
run_binop_string_float l BPlus r vm = return $ set_accu vm $ VString $ l ++ show r
run_binop_string_float _ BMinus _ _ = Left $ "Error: Cannot do string - float"
run_binop_string_float _ BMul _ _ = Left $ "Error: Cannot do string * float"
run_binop_string_float _ BDivide _ _ = Left $ "Error: Cannot do string / float"
run_binop_string_float _ BModulo _ _ = Left $ "Error: Cannot do string % float"
run_binop_string_float _ BEquals _ _ = Left $ "Error: Cannot do string == float"
run_binop_string_float _ BNotEquals _ _ = Left $ "Error: Cannot do string != float"
run_binop_string_float _ BLessThan _ _ = Left $ "Error: Cannot do string < float"
run_binop_string_float _ BGreaterThan _ _ = Left $ "Error: Cannot do string > float"
run_binop_string_float _ BLessThanEquals _ _ = Left $ "Error: Cannot do string <= float"
run_binop_string_float _ BGreaterThanEquals _ _ = Left $ "Error: Cannot do string >= float"
run_binop_string_float _ BAssing _ _ = Left $ "Error: Cannot do string = float"

run_binop_string_string :: String -> Binop -> String -> VM -> Either String VM
run_binop_string_string l BPlus r vm = return $ set_accu vm $ VString $ l ++ r
run_binop_string_string _ BMinus _ _ = Left $ "Error: Cannot do string - string"
run_binop_string_string _ BMul _ _ = Left $ "Error: Cannot do string * string"
run_binop_string_string _ BDivide _ _ = Left $ "Error: Cannot do string / string"
run_binop_string_string _ BModulo _ _ = Left $ "Error: Cannot do string % string"
run_binop_string_string l BEquals r vm = return $ set_accu vm $ VBool $ l == r
run_binop_string_string l BNotEquals r vm = return $ set_accu vm $ VBool $ l /= r
run_binop_string_string _ BLessThan _ _ = Left $ "Error: Cannot do string < string"
run_binop_string_string _ BGreaterThan _ _ = Left $ "Error: Cannot do string > string"
run_binop_string_string _ BLessThanEquals _ _ = Left $ "Error: Cannot do string <= string"
run_binop_string_string _ BGreaterThanEquals _ _ = Left $ "Error: Cannot do string >= string"
run_binop_string_string _ BAssing _ _ = Left $ "Error: Cannot do string = string"

run_binop_string_char :: String -> Binop -> Char -> VM -> Either String VM
run_binop_string_char l b r vm = run_binop_string_string l b [r] vm

run_binop_string_symbol :: String -> Binop -> String -> VM -> Either String VM
run_binop_string_symbol l b r vm = get_var vm r >>=
    \v -> run_binop (VString l) b (_vmvar_value v) vm

run_binop_string_bool :: String -> Binop -> Bool -> VM -> Either String VM
run_binop_string_bool l BPlus r vm = return $ set_accu vm $ VString $ l ++ show r
run_binop_string_bool _ BMinus _ _ = Left $ "Error: Cannot do string - bool"
run_binop_string_bool _ BMul _ _ = Left $ "Error: Cannot do string * bool"
run_binop_string_bool _ BDivide _ _ = Left $ "Error: Cannot do string / bool"
run_binop_string_bool _ BModulo _ _ = Left $ "Error: Cannot do string % bool"
run_binop_string_bool _ BEquals _ _ = Left $ "Error: Cannot do string == bool"
run_binop_string_bool _ BNotEquals _ _ = Left $ "Error: Cannot do string != bool"
run_binop_string_bool _ BLessThan _ _ = Left $ "Error: Cannot do string < bool"
run_binop_string_bool _ BGreaterThan _ _ = Left $ "Error: Cannot do string > bool"
run_binop_string_bool _ BLessThanEquals _ _ = Left $ "Error: Cannot do string <= bool"
run_binop_string_bool _ BGreaterThanEquals _ _ = Left $ "Error: Cannot do string >= bool"
run_binop_string_bool _ BAssing _ _ = Left $ "Error: Cannot do string = bool"

run_binop_char :: Char -> Binop -> Value -> VM -> Either String VM
run_binop_char l b (VNumber r) vm = run_binop_char_int l b r vm
run_binop_char l b (VFloating r) vm = run_binop_char_float l b r vm
run_binop_char l b (VString r) vm = run_binop_char_string l b r vm
run_binop_char l b (VChar r) vm = run_binop_char_char l b r vm
run_binop_char l b (VSymbol r) vm = run_binop_char_symbol l b r vm
run_binop_char l b (VBool r) vm = run_binop_char_bool l b r vm

run_binop_char_int :: Char -> Binop -> Int -> VM -> Either String VM
run_binop_char_int l BPlus r vm = return $ set_accu vm $ VString $ [l] ++ show r
run_binop_char_int _ BMinus _ _ = Left $ "Error: Cannot do char - int"
run_binop_char_int l BMul r vm = return $ set_accu vm $ VString $ concat $ replicate r [l]
run_binop_char_int _ BDivide _ _ = Left $ "Error: Cannot do char / int"
run_binop_char_int _ BModulo _ _ = Left $ "Error: Cannot do char % int"
run_binop_char_int _ BEquals _ _ = Left $ "Error: Cannot do char == int"
run_binop_char_int _ BNotEquals _ _ = Left $ "Error: Cannot do char != int"
run_binop_char_int _ BLessThan _ _ = Left $ "Error: Cannot do char < int"
run_binop_char_int _ BGreaterThan _ _ = Left $ "Error: Cannot do char > int"
run_binop_char_int _ BLessThanEquals _ _ = Left $ "Error: Cannot do char <= int"
run_binop_char_int _ BGreaterThanEquals _ _ = Left $ "Error: Cannot do char >= int"
run_binop_char_int _ BAssing _ _ = Left $ "Error: Cannot do char = int"

run_binop_char_float :: Char -> Binop -> Float -> VM -> Either String VM
run_binop_char_float l BPlus r vm = return $ set_accu vm $ VString $ [l] ++ show r
run_binop_char_float _ BMinus _ _ = Left $ "Error: Cannot do char - float"
run_binop_char_float _ BMul _ _ = Left $ "Error: Cannot do char * float"
run_binop_char_float _ BDivide _ _ = Left $ "Error: Cannot do char / float"
run_binop_char_float _ BModulo _ _ = Left $ "Error: Cannot do char % float"
run_binop_char_float _ BEquals _ _ = Left $ "Error: Cannot do char == float"
run_binop_char_float _ BNotEquals _ _ = Left $ "Error: Cannot do char != float"
run_binop_char_float _ BLessThan _ _ = Left $ "Error: Cannot do char < float"
run_binop_char_float _ BGreaterThan _ _ = Left $ "Error: Cannot do char > float"
run_binop_char_float _ BLessThanEquals _ _ = Left $ "Error: Cannot do char <= float"
run_binop_char_float _ BGreaterThanEquals _ _ = Left $ "Error: Cannot do char >= float"
run_binop_char_float _ BAssing _ _ = Left $ "Error: Cannot do char = float"

run_binop_char_string :: Char -> Binop -> String -> VM -> Either String VM
run_binop_char_string l b r vm = run_binop_string_string [l] b r vm

run_binop_char_char :: Char -> Binop -> Char -> VM -> Either String VM
run_binop_char_char l BPlus r vm = return $ set_accu vm $ VString $ [l] ++ [r]
run_binop_char_char _ BMinus _ _ = Left $ "Error: Cannot do char - char"
run_binop_char_char _ BMul _ _ = Left $ "Error: Cannot do char * char"
run_binop_char_char _ BDivide _ _ = Left $ "Error: Cannot do char / char"
run_binop_char_char _ BModulo _ _ = Left $ "Error: Cannot do char % char"
run_binop_char_char l BEquals r vm = return $ set_accu vm $ VBool $ l == r
run_binop_char_char l BNotEquals r vm = return $ set_accu vm $ VBool $ l /= r
run_binop_char_char l BLessThan r vm = return $ set_accu vm $ VBool $ l < r
run_binop_char_char l BGreaterThan r vm = return $ set_accu vm $ VBool $ l > r
run_binop_char_char l BLessThanEquals r vm = return $ set_accu vm $ VBool $ l <= r
run_binop_char_char l BGreaterThanEquals r vm = return $ set_accu vm $ VBool $ l >= r
run_binop_char_char _ BAssing _ _ = Left $ "Error: Cannot do char = char"

run_binop_char_symbol :: Char -> Binop -> String -> VM -> Either String VM
run_binop_char_symbol l b r vm = get_var vm r >>=
    \v -> run_binop (VChar l) b (_vmvar_value v) vm

run_binop_char_bool :: Char -> Binop -> Bool -> VM -> Either String VM
run_binop_char_bool l b r vm = run_binop_string_bool [l] b r vm

run_binop_symbol :: String -> Binop -> Value -> VM -> Either String VM
run_binop_symbol l BAssing r vm = set_var vm l r
run_binop_symbol l b r vm = get_var vm l >>=
    \v -> run_binop (_vmvar_value v) b r vm

run_binop_bool :: Bool -> Binop -> Value -> VM -> Either String VM
run_binop_bool l b (VNumber r) vm = run_binop_bool_int l b r vm
run_binop_bool l b (VFloating r) vm = run_binop_bool_float l b r vm
run_binop_bool l b (VString r) vm = run_binop_bool_string l b r vm
run_binop_bool l b (VChar r) vm = run_binop_bool_char l b r vm
run_binop_bool l b (VSymbol r) vm = run_binop_bool_symbol l b r vm
run_binop_bool l b (VBool r) vm = run_binop_bool_bool l b r vm

run_binop_bool_int :: Bool -> Binop -> Int -> VM -> Either String VM
run_binop_bool_int l b r vm = run_binop_int_int (_bool_to_int l) b r vm
    >>= _accu_as_bool

run_binop_bool_float :: Bool -> Binop -> Float -> VM -> Either String VM
run_binop_bool_float l b r vm = run_binop_float_float (_bool_to_float l) b r vm
    >>= _accu_as_bool

run_binop_bool_string :: Bool -> Binop -> String -> VM -> Either String VM
run_binop_bool_string l b r vm = run_binop_string_string (show l) b r vm
    >>= _accu_as_bool

run_binop_bool_char :: Bool -> Binop -> Char -> VM -> Either String VM
run_binop_bool_char l b r vm = run_binop_string_string (show l) b [r] vm
    >>= _accu_as_bool

run_binop_bool_symbol :: Bool -> Binop -> String -> VM -> Either String VM
run_binop_bool_symbol v b r vm =
    get_var vm r >>= \v' -> run_binop (VBool v) b (_vmvar_value v') vm

run_binop_bool_bool :: Bool -> Binop -> Bool -> VM -> Either String VM
run_binop_bool_bool l BPlus r vm =
    run_binop_int_int (_bool_to_int l) BPlus (_bool_to_int r) vm
    >>= _accu_as_bool