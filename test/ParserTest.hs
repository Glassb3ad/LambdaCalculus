module ParserTest 
(testParser)
where 

import Test.QuickCheck
import Grammar (Term (..), App(App), App'(..), Exp(..), stringifyTerm)
import ParserError (unexpectedLambdaErrorMessage, noMatchingParenthesisErrorMessage, rightParenthesisStartErrorMessage, ParseErr(..))
import Parser(parse, parseSourceCode)
import Lexeme (Lexeme (..), Token(..))

generateParserTest :: String -> String -> Property
generateParserTest = compareParseRes . parseSourceCode

compareParseRes :: Either [String] Term -> String -> Property
compareParseRes (Left erros) target = erros === [target]
compareParseRes (Right term) target = stringifyTerm term === target

generateParserErrorTest :: String -> [String] -> Property
generateParserErrorTest = compareParseErr . parseSourceCode

compareParseErr :: Either [String] Term -> [String] -> Property
compareParseErr (Left errors) target = errors === target
compareParseErr (Right term) target = [stringifyTerm term] === target

testParser :: IO ()
testParser = 
    do
        putStrLn "Testing parser with tokens..."
        quickCheck parserSuccess1
        quickCheck parserSuccess2
        quickCheck parserSuccess3
        quickCheck parserSuccess4
        quickCheck parserSuccess5
        putStrLn "Testing parser with source code..."
        quickCheck parseSourceCode1
        quickCheck parseSourceCode2
        quickCheck parseSourceCode3
        quickCheck parseSourceCode4
        quickCheck parseSourceCode5
        quickCheck parseSourceCode6
        quickCheck parseSourceCode7
        quickCheck parseSourceCode8
        quickCheck parseSourceCode9
        quickCheck parseSourceCode10
        quickCheck parseSourceCode11
        quickCheck parseSourceCode12
        quickCheck parseSourceCode13
        quickCheck parseSourceCode14
        quickCheck parseSourceCode15
        quickCheck parseSourceCode16
        quickCheck parseSourceCode17
        quickCheck parseSourceCode18
        quickCheck parseSourceCode19
        quickCheck parseSourceCode20
        quickCheck parseSourceCode21
        quickCheck parseSourceCode22
        quickCheck parseSourceCode23
        putStrLn "Testing failure handling..."
        quickCheck testParserErr1              
        quickCheck testParserErr2              
        quickCheck testParserErr3
        quickCheck testParserErr4
        quickCheck testParserErr5
        quickCheck testParserErr6
        quickCheck testParserErr7
        quickCheck parseSourceCodeError1
        quickCheck parseSourceCodeError2
        quickCheck parseSourceCodeError3
        quickCheck parseSourceCodeError4
        quickCheck parseSourceCodeError5

parserSuccess1 :: Property
parserSuccess1 = parse [Token (Lexeme.Variable "x") (0,0), Token (Lexeme.Variable "x") (1,1)] 
    === Right (TermApp 
           (App 
               (Grammar.Var (Lexeme.Variable "x")) 
               (App' (Grammar.Var (Lexeme.Variable "x")) EMPTY)
           ))

parserSuccess2 :: Property
parserSuccess2 = parse [Token (Lambda "x") (0,0), Token (Lexeme.Variable "x") (1,1), Token (Lexeme.Variable "x") (2,2)] 
    === Right (Abs 
            (Lambda "x")
            (TermApp 
                (App 
                    (Grammar.Var (Lexeme.Variable "x")) 
                    (App' (Grammar.Var (Lexeme.Variable "x")) EMPTY)
                )
            ))

parserSuccess3 :: Property
parserSuccess3 = parse [Token (Lexeme.Variable "x") (0,0), Token (Lexeme.Variable "x") (1,1), Token (Lexeme.Variable "x") (2,2)] 
                     === Right (TermApp (App 
                                (Grammar.Var (Lexeme.Variable "x")) 
                                (App' (Grammar.Var (Lexeme.Variable "x")) 
                                 (App' (Grammar.Var (Lexeme.Variable "x")) EMPTY)
                                )
                            ))

parserSuccess4 :: Property
parserSuccess4 = parse [Token LeftParenthesis (0,0), Token (Lambda "x") (1,1), Token (Lexeme.Variable "x") (2,2), Token RightParenthesis (3,3), Token (Lexeme.Variable "x") (4,4)] 
                     === Right (TermApp 
                            (App
                             (WithParenthesis 
                                (Abs
                                    (Lambda "x")
                                    (TermApp 
                                        (App
                                            (Grammar.Var (Lexeme.Variable "x")) 
                                            EMPTY
                                        )
                                    )
                                ))           
                             (App'
                             
                                (Grammar.Var (Lexeme.Variable "x"))
                                EMPTY
                             )
                            ))

parserSuccess5 :: Property
parserSuccess5 = parse [Token LeftParenthesis (0,0), Token (Lexeme.Variable "x") (1,1), Token RightParenthesis (3,3)] 
                      === Right (TermApp 
                             (App
                              (WithParenthesis 
                                     (TermApp 
                                         (App
                                             (Grammar.Var (Lexeme.Variable "x")) 
                                             EMPTY
                                         )
                                     )
                                )
                              EMPTY
                             ))

parseSourceCode1 :: Property
parseSourceCode1 = generateParserTest "xyz" "xyz"

parseSourceCode2 :: Property
parseSourceCode2 = generateParserTest "x y z" "x y z"

parseSourceCode3 :: Property
parseSourceCode3 = generateParserTest "(xy)z" "(xy) z"

parseSourceCode4 :: Property
parseSourceCode4 = generateParserTest "x(y)" "x (y)"

parseSourceCode5 :: Property
parseSourceCode5 = generateParserTest "λxy.v xy" "λxy. v xy"

parseSourceCode6 :: Property
parseSourceCode6 = generateParserTest "λt. λf. t" "λt.λf. t"

parseSourceCode7 :: Property
parseSourceCode7 = generateParserTest "λp. p (λt.λf. t)" "λp. p (λt.λf. t)"

parseSourceCode8 :: Property
parseSourceCode8 = generateParserTest "λf.λs.λb. b f s" "λf.λs.λb. b f s"

parseSourceCode9 :: Property
parseSourceCode9 = generateParserTest "λs. λz. z" "λs.λz. z"

parseSourceCode10 :: Property
parseSourceCode10 = generateParserTest "λs. λz. s z" "λs.λz. s z"

parseSourceCode11 :: Property
parseSourceCode11 = generateParserTest "λs.λz. s (s z)" "λs.λz. s (s z)"

parseSourceCode12 :: Property
parseSourceCode12 = generateParserTest "λs.λz. s (s (s z))" "λs.λz. s (s (s z))"

parseSourceCode13 :: Property
parseSourceCode13 = generateParserTest "λs.λz. s (s (s (s z)))" "λs.λz. s (s (s (s z)))"

parseSourceCode14 :: Property
parseSourceCode14 = generateParserTest "λxy.  v    xy" "λxy. v xy"

parseSourceCode15 :: Property
parseSourceCode15 = generateParserTest "λx.λy.x y" "λx.λy. x y"

parseSourceCode16 :: Property
parseSourceCode16 = generateParserTest "λxλy x y" "λx.λy. x y"

parseSourceCode17 :: Property
parseSourceCode17 = generateParserTest "λy. (λx x) y" "λy. (λx. x) y"

parseSourceCode18 :: Property
parseSourceCode18 = generateParserTest "(λx. y x) (λy y)" "(λx. y x) (λy. y)"

parseSourceCode19 :: Property
parseSourceCode19 = generateParserTest "(  ( (  x )  )        )" "(((x)))"

parseSourceCode20 :: Property
parseSourceCode20 = generateParserTest "(λx.x)" "(λx. x)"

parseSourceCode21 :: Property
parseSourceCode21 = generateParserTest "λm. λn. λs. λz. m s (n s z)" "λm.λn.λs.λz. m s (n s z)"

parseSourceCode22 :: Property
parseSourceCode22 = generateParserTest 
    "λm. λn. m ((λm.λn.λs.λz. m s (n s z)) n) (λs.λz. z)" 
    "λm.λn. m ((λm.λn.λs.λz. m s (n s z)) n) (λs.λz. z)"

parseSourceCode23 :: Property
parseSourceCode23 = generateParserTest "λf. (λx. f (x x)) (λx. f (x x))" "λf. (λx. f (x x)) (λx. f (x x))"

testParserErr1 :: Property
testParserErr1 = parse [Token LeftParenthesis (0,0), Token (Lexeme.Variable "x") (1,1)] 
                     === Left [NoMatchingParenthesis (Token LeftParenthesis (0,0))]

testParserErr2 :: Property
testParserErr2 = parse [Token LeftParenthesis (0,0), Token (Lexeme.Variable "x") (1,1), Token LeftParenthesis (2,2), Token (Lexeme.Variable "x") (3,3)] 
                     === Left [NoMatchingParenthesis (Token LeftParenthesis (2,2))]

testParserErr3 :: Property
testParserErr3 = parse [Token LeftParenthesis (0,0), Token (Lexeme.Variable "x") (1,1), Token LeftParenthesis (2,2), Token (Lexeme.Variable "x") (3,3), Token RightParenthesis (4,4)] 
                     === Left [NoMatchingParenthesis (Token LeftParenthesis (0,0))]

testParserErr4 :: Property
testParserErr4 = parse [Token LeftParenthesis (0,0), Token (Lexeme.Variable "x") (1,1), Token (Lexeme.Variable "x") (2,2), Token LeftParenthesis (3,3)] 
                     === Left [NoMatchingParenthesis (Token LeftParenthesis (3,3))]

testParserErr5 :: Property
testParserErr5 = parse [Token LeftParenthesis (0,0), Token (Lexeme.Variable "x") (1,1), Token RightParenthesis (2,2), Token (Lambda "x") (3,3), Token (Lexeme.Variable "x") (4,4)] 
                     === Left [UnexpectedLambda (Token (Lambda "x") (3,3))]

testParserErr6 :: Property
testParserErr6 = parse [Token LeftParenthesis (0,0), Token (Lexeme.Variable "x") (1,1), Token RightParenthesis (2,2), Token (Lambda "x") (3,3), Token LeftParenthesis (4,4), Token (Lexeme.Variable "x") (5,5)] 
                     === Left [UnexpectedLambda (Token (Lambda "x") (3,3)), NoMatchingParenthesis (Token LeftParenthesis (4,4))]

testParserErr7 :: Property
testParserErr7 = parse [Token (Lexeme.Variable "x") (0,0), Token RightParenthesis (1,1)] 
                     === Left [RightParenthesisStart (Token RightParenthesis (1,1))]

parseSourceCodeError1 :: Property
parseSourceCodeError1 = generateParserErrorTest "(λm. n) λm. n" [unexpectedLambdaErrorMessage 8 10 "(λm. n) λm. n"]

parseSourceCodeError2 :: Property
parseSourceCodeError2 = generateParserErrorTest "(λm. n λm. n" [unexpectedLambdaErrorMessage 7 9 "(λm. n λm. n"]

parseSourceCodeError3 :: Property
parseSourceCodeError3 = generateParserErrorTest "(λm. n (λm. n)" [noMatchingParenthesisErrorMessage 0 0 "(λm. n (λm. n)"]

parseSourceCodeError4 :: Property
parseSourceCodeError4 = generateParserErrorTest "(λm. n) λm. (n" [unexpectedLambdaErrorMessage 8 10 "(λm. n) λm. (n", noMatchingParenthesisErrorMessage 12 12 "(λm. n) λm. (n"]

parseSourceCodeError5 :: Property
parseSourceCodeError5 = generateParserErrorTest ")λm. n λm. (n" [rightParenthesisStartErrorMessage 0 0 ")λm. n λm. (n", unexpectedLambdaErrorMessage 7 9 ")λm. n λm. (n", noMatchingParenthesisErrorMessage 11 11 ")λm. n λm. (n"]