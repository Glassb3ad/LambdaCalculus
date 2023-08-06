module ScannerTest 
(testScanner)
where 

import Test.QuickCheck
import Scanner (scanner, Token (Token) )
import Lexeme (Lexeme (LeftParenthesis, RightParenthesis, Lambda, Variable), scanNextLexeme)

testScanner :: IO ()
testScanner = do 
              putStrLn "Testing scanner..."
              quickCheck testScanNextLexeme1
              quickCheck testScanNextLexeme2
              quickCheck testScanNextLexeme3
              quickCheck testScanNextLexeme4
              quickCheck testScanNextLexeme5
              quickCheck testScanNextLexeme6

              quickCheck testScannerSuccess1
              quickCheck testScannerSuccess2
              quickCheck testScannerSuccess3
              quickCheck testScannerSuccess4
              quickCheck testScannerSuccess5
              quickCheck testScannerSuccess6

              quickCheck testScannerFailure1
              quickCheck testScannerFailure2
              putStrLn "Scanner tested"

testScanNextLexeme1 :: Property
testScanNextLexeme1 = scanNextLexeme "xyz" 0 === Right (Token (Variable "xyz") (0,2))

testScanNextLexeme2 :: Property
testScanNextLexeme2 = scanNextLexeme "x y" 0 === Right (Token (Variable "x") (0,0))

testScanNextLexeme3 :: Property
testScanNextLexeme3 = scanNextLexeme "(xy)z" 0 === Right (Token LeftParenthesis (0,0))

testScanNextLexeme4 :: Property
testScanNextLexeme4 = scanNextLexeme ")xy(z" 0 === Right (Token RightParenthesis (0,0))

testScanNextLexeme5 :: Property
testScanNextLexeme5 = scanNextLexeme "λxy.v xy" 0 === Right (Token (Lambda "xy") (0,3))

testScanNextLexeme6 :: Property
testScanNextLexeme6 = scanNextLexeme "λxy v xy" 0 === Right (Token (Lambda "xy") (0,2))

testScannerSuccess1 :: Property
testScannerSuccess1 = scanner "x y" === Right [Token (Variable "x") (0,0), Token (Variable "y") (2,2)]

testScannerSuccess2 :: Property
testScannerSuccess2 = scanner "(x y)" === Right [Token LeftParenthesis (0,0),Token (Variable "x") (1,1),Token (Variable "y") (3,3),Token RightParenthesis (4,4)]

testScannerSuccess3 :: Property
testScannerSuccess3 = scanner "λxy.v xy" === Right [Token (Lambda "xy") (0,3), Token (Variable "v") (4,4), Token (Variable "xy") (6,7)]

testScannerSuccess4 :: Property
testScannerSuccess4 = scanner "x (y x)" === Right [Token (Variable "x") (0,0), Token LeftParenthesis (2,2),  Token (Variable "y") (3,3), Token (Variable "x") (5,5), Token RightParenthesis (6,6)]

testScannerSuccess5 :: Property
testScannerSuccess5 = scanner "λx. x (y x)" === Right [Token (Lambda "x") (0,2), Token (Variable "x") (4,4), Token LeftParenthesis (6,6),  Token (Variable "y") (7,7), Token (Variable "x") (9,9), Token RightParenthesis (10,10)]

testScannerSuccess6 :: Property
testScannerSuccess6 = scanner "  x y " === Right [Token (Variable "x") (2,2), Token (Variable "y") (4,4)]

testScannerFailure1 :: Property
testScannerFailure1 = scanner "5" === Left "Lexical error at 0: \x1b[91m5\x1b[0m"

testScannerFailure2 :: Property
testScannerFailure2 = scanner "((xy)5)" === Left "Lexical error at 5: ((xy)\x1b[91m5\x1b[0m)"