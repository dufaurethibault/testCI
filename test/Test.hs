module Main (main) where
import Test.HUnit
import System.Exit

import MarcelDef
import MarcelOperations
import MarcelVM
import MarcelVMDef
import MarcelParser
import MarcelAst

      --Testing MARCELPARSER FILE

teststartsWithfirstElemEmpty = TestCase (
  assertEqual "" True (startsWith "" "test")
  )

teststartsWithsecondElemEmpty = TestCase (
  assertEqual "" False (startsWith "test" "")
  )

teststartsWithTrue = TestCase (
  assertEqual "" True (startsWith "te" "test")
  )

teststartsWithFalse = TestCase (
  assertEqual "" False (startsWith "ta" "test")
  )

testgetTokenWithNothing = TestCase (
  assertEqual ""(Left("", "") ) (getToken "" ["test", "test"])
  )

testgetTokenWithNoStringList = TestCase (
  assertEqual "" (Left ("", "")) (getToken "test" [])
  )

testgetTokenWithLastPlace = TestCase (
  assertEqual "" (Right ("test", "")) (getToken "test" ["bonjour", "je", "test"])
  )

testgetTokenWithfirstPlace = TestCase (
  assertEqual "" (Right ("test", "")) (getToken "test" ["test", "bonjour", "je"])
  )

--Testing tokenizeMarcelToken

testtokenizeMarcelToken = TestCase (
      assertEqual "" (Right ["+", "-", "==", "*"])
      (tokenizeMarcelToken "+ - == *")
  )

testtokenizeMarcelTokenEmpty = TestCase (
      assertEqual "" (Right [])
      (tokenizeMarcelToken "")
  )

testmarcelStringParserInvalidToken = TestCase (
      assertEqual "" (Left ("2", "Invalid symbol: [2]"))
      (tokenizeMarcelToken "2 + 2")
  )

--Testing tokenizeMarcelNumber

testtokenizeMarcelNumber = TestCase (
    assertEqual "tokenizeMarcelNumber \"123\"" (Right ["123"]) (tokenizeMarcelNumber "123")
  )
  
testTokenizeMarcelNegativeNumber = TestCase (
    assertEqual "tokenizeMarcelNumber \"-123\"" (Right ["-123"]) (tokenizeMarcelNumber "-123")
  )

testTokenizeMarcelZeroNumber = TestCase (
    assertEqual "tokenizeMarcelNumber \"0\"" (Right ["0"]) (tokenizeMarcelNumber "0")
  )
  
testTokenizeMarcelNegativeZeroNumber = TestCase (
    assertEqual "tokenizeMarcelNumber \"-0\"" (Right ["-0"]) (tokenizeMarcelNumber "-0")
  )

--Testing tokenizeMarcelSpace

testTokenizeMarcelEmptySpace = TestCase (
    assertEqual "" (Right []) (tokenizeMarcelSpace "")
  )

testTokenizeMarcelSingleSpace = TestCase (
    assertEqual "" (Right []) (tokenizeMarcelSpace " ")
  )

testTokenizeMarcelMultipleSpace = TestCase (
    assertEqual "" (Right []) (tokenizeMarcelSpace "  \t \n ")
  )

testTokenizeMarcelNonSpaceInput = TestCase (
    assertEqual "" (marcelStringParserInvalidChar "a") (tokenizeMarcelSpace "a")
  )

--Testing tokenizeMarcelSymbol

testTokenizeMarcelSymbolEmptyString = TestCase (
    assertEqual "" (Right []) (tokenizeMarcelSymbol "")
  )

testTokenizeMarcelSymbolInvalidStartSymbol = TestCase (
    assertEqual "" (marcelStringParserInvalidSymbol "+") (tokenizeMarcelSymbol "+")
  )

testTokenizeMarcelSymbolValidSymbol = TestCase (
    assertEqual "" (Right ["a", "+", "b"]) (tokenizeMarcelSymbol "a + b")
  )

    --Testing MARCELAST FILE

testEmptyInput :: Test
testEmptyInput = TestCase $ assertEqual
  "Empty input should return an error"
  (Left "Error: Expected function name but is EOF")
  (marcelTokensAsFunc [])

  
testmarcelTokensAsFuncArgs'emptylist :: Test
testmarcelTokensAsFuncArgs'emptylist = TestCase $ assertEqual
        "Empty argument list should return empty list"
        (Right ([], []))
        (marcelTokensAsFuncArgs' [])

testmarcelTokensAsFuncArgs'missing :: Test
testmarcelTokensAsFuncArgs'missing = TestCase $ assertEqual
        "Missing right paren should return error"
        (Right (["x"],[TLeftBracket,TComma,TSymbol "y"]))
        (marcelTokensAsFuncArgs' [TSymbol "x", TLeftBracket , TComma, TSymbol "y"])

testmarcelTokensAsFuncArgs'onearg :: Test
testmarcelTokensAsFuncArgs'onearg = TestCase $ assertEqual 
        "Single argument should return list with one element"
        (Right (["x"], [TRightParen]))
        (marcelTokensAsFuncArgs' [TSymbol "x", TRightParen])

testmarcelTokensAsFuncArgs'multiple :: Test
testmarcelTokensAsFuncArgs'multiple = TestCase $ assertEqual
        "Multiple arguments should return list with multiple elements"
        (Right (["x"],[TComma,TSymbol "y",TComma,TSymbol "z",TRightParen]))
        (marcelTokensAsFuncArgs' [TSymbol "x", TComma, TSymbol "y", TComma, TSymbol "z", TRightParen])

testmarcelTokensAsFuncArgsemptylist :: Test
testmarcelTokensAsFuncArgsemptylist = TestCase $ assertEqual
        "Empty input should return an error"
        (Left "Error: Expected function args but is EOF")
        (marcelTokensAsFuncArgs [])

testmarcelTokensAsFuncArgsmissing :: Test
testmarcelTokensAsFuncArgsmissing = TestCase $ assertEqual
        "Missing left bracket should return an error"
        (Left "Error: Expected TLeftBracket, got TSymbol \"function\"")
        (marcelTokensAsFuncArgs [TSymbol "function"])

testmarcelAsAst :: Test
testmarcelAsAst = TestCase $ assertEqual
        "Valid input should return a function AST"
        (Left "Error: Expected function but got: [TNumber 5]")
        (marcelAsAst [TNumber 5])

main :: IO ()
main = do
  counts <- runTestTT (test [
    teststartsWithfirstElemEmpty,
    teststartsWithsecondElemEmpty,
    teststartsWithTrue,
    teststartsWithFalse,
    testgetTokenWithNothing,
    testgetTokenWithNoStringList,
    testgetTokenWithLastPlace,
    testgetTokenWithfirstPlace,
    testtokenizeMarcelToken,
    testtokenizeMarcelTokenEmpty,
    testmarcelStringParserInvalidToken,
    testTokenizeMarcelNegativeNumber,
    testTokenizeMarcelNegativeZeroNumber,
    testTokenizeMarcelEmptySpace,
    testTokenizeMarcelSingleSpace,
    testTokenizeMarcelMultipleSpace,
    testTokenizeMarcelNonSpaceInput,
    testTokenizeMarcelSymbolEmptyString,
    testTokenizeMarcelSymbolInvalidStartSymbol,
    testTokenizeMarcelSymbolValidSymbol,
    testEmptyInput,
    testmarcelTokensAsFuncArgs'emptylist,
    testmarcelTokensAsFuncArgs'onearg,
    testmarcelTokensAsFuncArgs'multiple,
    testmarcelTokensAsFuncArgs'missing,
    testmarcelTokensAsFuncArgsemptylist,
    testmarcelTokensAsFuncArgsmissing,
    testmarcelAsAst
    ])
  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure