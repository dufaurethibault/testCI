module MarcelParser (
    tokenizeMarcel,
    transformMarcelTokens,
    (<|>),
    startsWith,
    getToken,
    tokenizeMarcelToken,
    tokenizeMarcelNumber,
    tokenizeMarcelSpace,
    tokenizeMarcelSymbol,
    marcelStringParserInvalidSymbol,
    marcelStringParserInvalidChar,
    marcelStringParserInvalidNumber,
    marcelStringParserInvalidString,
    marcelStringParserInvalidToken,

) where

{-# OPTIONS_GHC -WEverything #-}

import MarcelDef (
    MarcelStringParser,
    MarcelStringParserResult,
    Binop(..),
    MarcelToken(..)
    )

import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)

-- This function choses between two results
-- If the first result is a Left, it returns the second result
-- If the first result is a Right, it returns the first result
-- This is basically an or operator for Either
(<|>) :: Either a b -> Either a b -> Either a b
(<|>) (Left _) x = x
(<|>) x _ = x

marcelBinops :: [(String, Binop)]
marcelBinops = [("+", BPlus), ("-", BMinus), ("*", BMul),
                ("/", BDivide), ("%", BModulo),
                ("==", BEquals), ("!=", BNotEquals),
                ("<=", BLessThanEquals), (">=", BGreaterThanEquals),
                ("<", BLessThan), (">", BGreaterThan),
                ("=", BAssing)]


marcelTokensUtils :: [(String, MarcelToken)]
marcelTokensUtils = map (\(x, y) -> (x, TBinop y)) marcelBinops ++
    [("(", TLeftParen), (")", TRightParen), ("{", TLeftBrace),
    ("}", TRightBrace), ("[", TLeftBracket), ("]", TRightBracket),
    (",", TComma), (";", TSemicolon), ("if", TIf), ("si", TIf),
    ("return", TReturn), ("retourne", TReturn),
    ("elseif", TElseIf), ("sinonsi", TElseIf), ("else", TElse),
    ("sinon", TElse), ("while", TWhile), ("tanke", TWhile),
    ("function", TFunc), ("fonction", TFunc), ("debut", TBegin),
    ("begin", TBegin), ("fin", TEnd), ("end", TEnd), ("break", TBreak),
    ("arrete", TBreak), ("for", TFor), ("pour", TFor),
    (".", TDot), ("true", TBool True), ("faux", TBool False),
    ("false", TBool False), ("vrai", TBool True)]

transformMarcelTokens :: [String] -> [MarcelToken]
transformMarcelTokens [] = []
transformMarcelTokens ("-":xs) = TBinop BMinus : transformMarcelTokens xs
transformMarcelTokens (('-':x:xs):ys) =
    TNumber (read ('-':x:xs)) : transformMarcelTokens ys
transformMarcelTokens (('"':xs):ys) =
    TString (takeWhile (/= '"') xs) : transformMarcelTokens ys
transformMarcelTokens (('\'':'\\':x:_):ys) = TChar x : transformMarcelTokens ys
transformMarcelTokens (('\'':xs:_):ys) = TChar xs : transformMarcelTokens ys
transformMarcelTokens ((x:xs):ys)
    | isDigit x = TNumber (read (x:xs)) : transformMarcelTokens ys
transformMarcelTokens (x:xs) = case lookup x marcelTokensUtils of
    Just y -> y : transformMarcelTokens xs
    Nothing -> TSymbol x : transformMarcelTokens xs

marcelStringParserEOF :: MarcelStringParserResult
marcelStringParserEOF = Left ("", "Unexpected end of input")

marcelStringParserInvalidChar :: String -> MarcelStringParserResult
marcelStringParserInvalidChar (x:xs) =
    Left (x:xs, "Invalid character: [" ++ [x] ++ "]")
marcelStringParserInvalidChar _ = marcelStringParserEOF

marcelStringParserNonFinishedString :: String -> MarcelStringParserResult
marcelStringParserNonFinishedString (x:xs) =
    Left (x:xs, "Non finished string")
marcelStringParserNonFinishedString _ = marcelStringParserEOF

marcelStringParserInvalidString :: String -> MarcelStringParserResult
marcelStringParserInvalidString (x:xs) =
    Left (x:xs, "Invalid string: [" ++ [x] ++ "]")
marcelStringParserInvalidString _ = marcelStringParserEOF

marcelStringParserInvalidToken :: String -> MarcelStringParserResult
marcelStringParserInvalidToken (x:xs) =
    Left (head $ words (x:xs), "Invalid token: [" ++ [x] ++ "]")
marcelStringParserInvalidToken _ = marcelStringParserEOF

marcelStringParserInvalidNumber :: String -> MarcelStringParserResult
marcelStringParserInvalidNumber (x:xs) =
    Left (x:xs, "Expected either '-' or a valid number but got: [" ++
          [x] ++ "]")
marcelStringParserInvalidNumber _ = marcelStringParserEOF

marcelStringParserInvalidSymbol :: String -> MarcelStringParserResult
marcelStringParserInvalidSymbol (x:xs) =
    Left (head $ words (x:xs), "Invalid symbol: [" ++ [x] ++ "]")
marcelStringParserInvalidSymbol _ = marcelStringParserEOF

tokenizeMarcelSpace :: String -> MarcelStringParserResult
tokenizeMarcelSpace "" = Right []
tokenizeMarcelSpace (' ':xs) = tokenizeMarcel xs
tokenizeMarcelSpace ('\t':xs) = tokenizeMarcel xs
tokenizeMarcelSpace ('\n':xs) = tokenizeMarcel xs
tokenizeMarcelSpace ('\r':xs) = tokenizeMarcel xs
tokenizeMarcelSpace x = marcelStringParserInvalidChar x

startsWith :: String -> String -> Bool
startsWith "" _ = True
startsWith _ "" = False
startsWith (x:xs) (y:ys) = x == y && startsWith xs ys

endsWith :: String -> String -> Bool
endsWith s e = startsWith (reverse s) (reverse e)

-- predicates are MarcelTokens
getToken :: String -> [String] -> Either (String, String) (String, String)
getToken "" _ = Left ("", "")
getToken x [] = Left ("", "")
getToken x (y:ys)
    | startsWith y x = return $ (y, drop (length y) x)
    | otherwise = getToken x ys

marcelTokens :: [String]
marcelTokens = map fst marcelBinops ++
    ["(", ")", "{", "}", ";", "[", "]", ",", "."]

-- If the token exists in the list of symbols, return it
tokenizeMarcelToken :: String -> MarcelStringParserResult
tokenizeMarcelToken "" = Right []
tokenizeMarcelToken x = ((getToken x marcelTokens) >>=
    \(token, rest) -> tokenizeMarcel rest >>=
    \result -> return $ token : result)
    <|> (tokenizeMarcelSymbol x)

tokenizeMarcelNumber :: String -> MarcelStringParserResult
tokenizeMarcelNumber ('-':xs) = (tokenizeMarcel $ dropWhile isDigit xs)
        >>= \rest -> return $ ('-':num) : rest
    where num = takeWhile isDigit xs
tokenizeMarcelNumber (x:xs)
        | isDigit x = (tokenizeMarcelNumber ('-':x:xs))
            >>= \rest -> return $ (tail $ head $ rest):tail rest
        | otherwise = marcelStringParserInvalidNumber (x:xs)

evaluateString :: String -> String
evaluateString ('\\':'n':xs) = '\n':evaluateString xs
evaluateString ('\\':'t':xs) = '\t':evaluateString xs
evaluateString ('\\':'\\':xs) = '\\':evaluateString xs
evaluateString (x:xs) = x:evaluateString xs
evaluateString "" = ""

tokenizeMarcelString :: String -> MarcelStringParserResult
tokenizeMarcelString ('"':x)
    | null rest = marcelStringParserNonFinishedString x
    | otherwise = tokenizeMarcel rest' 
        >>= \result -> return $ ('"':(evaluateString str) ++ "\"") : result
        where
            (str, rest) = break (== '"') x
            rest' = drop 1 rest
tokenizeMarcelString x = marcelStringParserInvalidString x

isValidStartSymbolChar :: Char -> Bool
isValidStartSymbolChar x = isAlpha x || x == '_'

isValidSymbolChar :: Char -> Bool
isValidSymbolChar x = isAlphaNum x || x == '_'

tokenizeMarcelSymbol :: String -> MarcelStringParserResult
tokenizeMarcelSymbol (x:xs)
    | isValid = (tokenizeMarcel $ dropWhile isValidSymbolChar xs)
        >>= \rest' -> return $ (x:sym) : rest'
    | otherwise = marcelStringParserInvalidSymbol (x:xs)
    where
        sym = takeWhile isValidSymbolChar xs
        rest = dropWhile isValidSymbolChar xs
        isValid = isValidStartSymbolChar x
tokenizeMarcelSymbol "" = Right []

tokenizeMarcelChar :: String -> MarcelStringParserResult
tokenizeMarcelChar ('\'':x:'\'':xs) =  tokenizeMarcel xs >>=
    \rest -> return $ ('\'':x:'\'':[]) : rest
tokenizeMarcelChar ('\'':'\\':'n':'\'':xs) = tokenizeMarcel xs >>=
    \rest -> return $ "'\n'" : rest
tokenizeMarcelChar ('\'':'\\':'t':'\'':xs) = tokenizeMarcel xs >>=
    \rest -> return $ "'\t'" : rest
tokenizeMarcelChar x = marcelStringParserInvalidChar x

_marcelParser :: [(String, (String -> MarcelStringParserResult))]
_marcelParser = [("tokenizeMarcelSpace", tokenizeMarcelSpace),
                ("tokenizeMarcelNumber", tokenizeMarcelNumber),
                ("tokenizeMarcelToken", tokenizeMarcelToken),
                ("tokenizeMarcelChar", tokenizeMarcelChar),
                ("tokenizeMarcelString", tokenizeMarcelString),
                ("tokenizeMarcelSymbol", tokenizeMarcelSymbol)
                ]

-- Gets the parsers functions in _marcelParser
marcelParsers :: [String -> MarcelStringParserResult]
marcelParsers = map snd _marcelParser

-- Gets the names of the parsers in _marcelParser
marcelParserNames :: [String]
marcelParserNames = map fst _marcelParser

tokenizeEither :: String -> [MarcelStringParser] -> MarcelStringParserResult
tokenizeEither "" _ = Right []
tokenizeEither x (f:fs) = f x <|> tokenizeEither x fs
tokenizeEither x [] = Left ("", "Compiler error: No parsers provided")

post_process_types :: [String] -> [String]
post_process_types [] = []
post_process_types ("entier":xs) = "int":post_process_types xs
post_process_types ("floattant":xs) = "float":post_process_types xs
post_process_types ("caracteres":xs) = "string":post_process_types xs
post_process_types ("caractere":xs) = "char":post_process_types xs
post_process_types ("booleen":xs) = "bool":post_process_types xs
post_process_types ("tout":xs) = "any":post_process_types xs
post_process_types (x:xs) = x:post_process_types xs

-- Tokenize with any of the parsers in marcelParsers
tokenizeMarcel :: String -> MarcelStringParserResult
tokenizeMarcel "" = Right []
tokenizeMarcel x = (tokenizeEither x marcelParsers)
    >>= \result -> return $ post_process_types result
