module Parser (parse, parseSourceCode)
where

import ParserError (parserErrorMessages, ParseErr(..))
import Grammar (Term (Abs, TermApp), App(App), App'(App', EMPTY), Exp(Var, WithParenthesis))
import Lexeme (Lexeme (..), Token(..))
import Scanner (scanner)
import Data.Bifunctor (first)

type ParseRes a = Either [ParseErr] a

parseSourceCode :: String -> Either [String] Term
parseSourceCode source = (parseScannerResults source . scanner) source

parseScannerResults :: String -> Either String [Token] -> Either [String] Term
parseScannerResults source (Right tokens) = (handleParserErrors source . parse) tokens
parseScannerResults _ (Left error) = Left [error]

handleParserErrors :: String -> Either [ParseErr] Term -> Either [String] Term
handleParserErrors source (Left errors) = Left (parserErrorMessages source errors)
handleParserErrors _ (Right term) = Right term

ifNotError :: (ParseRes a, [Token]) -> ((a, [Token]) -> (ParseRes b, [Token])) -> (ParseRes b, [Token])
ifNotError (Left err, tokens) _ = (Left err, tokens)
ifNotError (Right x, tokens) f = f (x, tokens)

recover :: (ParseRes Term, [Token]) -> [ParseErr] ->  (ParseRes Term , [Token])
recover (Left errors, []) prevErrors = (Left (prevErrors ++ errors), [])
recover (Left errors, tokens) prevErrors = recover (parseTerm tokens False) (prevErrors ++ errors)
recover (Right _, tokens) prevErrors = (Left prevErrors, tokens)

parse :: [Token] -> Either [ParseErr] Term
parse tokens = fst (parseTerm tokens False)

parseTerm :: [Token] -> Bool -> (ParseRes Term, [Token])
parseTerm ((Token (Lambda x) _):tokens) leftPar = ifNotError (parseTerm tokens leftPar) (first (\y -> Right (Abs (Lambda x) y)))
parseTerm tokens leftPar = handleParseAppErrors (parseApp tokens leftPar)

handleParseAppErrors :: (ParseRes App, [Token]) -> (ParseRes Term, [Token])
handleParseAppErrors (Right app, tokens) = (Right (TermApp app), tokens)
handleParseAppErrors (Left errs, tokens) = recover (Left errs, tokens) []

parseApp :: [Token] -> Bool -> (ParseRes App, [Token])
parseApp tokens leftPar = ifNotError (parseExp tokens) (\x ->
    if leftPar && isRightParenthesisNext (snd x)
        then (Right (App (fst x) EMPTY), snd x)
        else ifNotError (parseApp' (snd x) leftPar) (first (\y -> Right (App (fst x) y)))
    )

parseApp' :: [Token] -> Bool -> (ParseRes App', [Token])
parseApp' [] _ = (Right EMPTY, [])
parseApp' tokens leftPar = 
    ifNotError (parseExp tokens) (\x ->
    if leftPar && isRightParenthesisNext (snd x) 
        then (Right (App' (fst x) EMPTY), snd x)
        else ifNotError (parseApp' (snd x) leftPar) (first (\y -> Right ( App' (fst x) y)))
    )

parseExp :: [Token] -> (ParseRes Exp, [Token])
parseExp ((Token (Lambda x) pos):tokens) = (Left [UnexpectedLambda (Token (Lambda x) pos)], tokens)
parseExp ((Token (Lexeme.Variable x) _):tokens) = (Right (Grammar.Var (Lexeme.Variable x)), tokens)
parseExp ((Token RightParenthesis pos):tokens) = (Left [RightParenthesisStart (Token RightParenthesis pos)], tokens)
parseExp [Token LeftParenthesis pos] = (Left [NoMatchingParenthesis (Token LeftParenthesis pos)], [])
parseExp ((Token LeftParenthesis pos):tokens) = ifNotError (parseTerm tokens True) (\x ->
    if isRightParenthesisNext (snd x)
        then (Right (WithParenthesis (fst x)), tail (snd x))
        else (Left [NoMatchingParenthesis (Token LeftParenthesis pos)], tokens)
    )
parseExp [] = (Left [End], [])

isRightParenthesisNext :: [Token] -> Bool
isRightParenthesisNext (Token RightParenthesis _:_) = True 
isRightParenthesisNext _ = False