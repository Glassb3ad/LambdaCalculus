module ScannerTest 
(testScanner)
where 

import Test.QuickCheck
import Scanner (scanner, Token (Token), )
import Lexeme (Lexeme (LeftParenthesis, RightParenthesis, Lambda, Variable), scanNextLexeme)

testScanner :: IO ()
testScanner = do 
              putStrLn "Testing scanner"
              verboseCheck testScanNextLexeme1
              verboseCheck testScanNextLexeme2
              verboseCheck testScanNextLexeme3
              verboseCheck testScanNextLexeme4
              verboseCheck testScanNextLexeme5
              verboseCheck testScanNextLexeme6

              verboseCheck testScannerSuccess1
              verboseCheck testScannerSuccess2
              verboseCheck testScannerSuccess3
              verboseCheck testScannerSuccess4
              verboseCheck testScannerSuccess5
              verboseCheck testScannerSuccess6
              putStrLn "Scanner testing ended"

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
