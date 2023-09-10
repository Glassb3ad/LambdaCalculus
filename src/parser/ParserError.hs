module ParserError (parserErrorMessages, unexpectedLambdaErrorMessage,
noMatchingParenthesisErrorMessage, rightParenthesisStartErrorMessage,
undefinedErrorMessage, ParseErr(..))
where

import Lexeme (Token(..))
import Utils (colorSubstringWithRed)

data ParseErr = NoMatchingParenthesis Token | UnexpectedLambda Token | RightParenthesisStart Token | End
    deriving ( Eq, Show )

parserErrorMessages :: String -> [ParseErr] -> [String]
parserErrorMessages source = map (parserErrorMessage source)

parserErrorMessage :: String -> ParseErr -> String
parserErrorMessage source (NoMatchingParenthesis (Token _ (start, end))) = noMatchingParenthesisErrorMessage start end source
parserErrorMessage source (UnexpectedLambda (Token _ (start, end))) = unexpectedLambdaErrorMessage start end source
parserErrorMessage source (RightParenthesisStart (Token _ (start, end))) = rightParenthesisStartErrorMessage start end source
parserErrorMessage _ _ = undefinedErrorMessage

unexpectedLambdaErrorMessage :: Int -> Int -> String -> String
unexpectedLambdaErrorMessage start end source = 
    "Parse error: unexpected lambda at " ++ show start ++ ": " ++ colorSubstringWithRed start end source

noMatchingParenthesisErrorMessage :: Int -> Int -> String -> String
noMatchingParenthesisErrorMessage start end source =
    "Parse error: no matching parenthesis found for parenthesis at " ++ show start ++ ": " ++ colorSubstringWithRed start end source

rightParenthesisStartErrorMessage :: Int -> Int -> String -> String
rightParenthesisStartErrorMessage start end source =
    "Parse error: expression starting with right parenthesis at " ++ show start ++ ": " ++ colorSubstringWithRed start end source

undefinedErrorMessage :: String
undefinedErrorMessage = "Parse error: unidentified parser error (source code ended before parsing was completed)"
