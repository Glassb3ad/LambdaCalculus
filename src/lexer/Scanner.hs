module Scanner 
(scanner, Token (Token))
where 

import Lexeme (Lexeme, Position, LexicalError, Token(Token), scanNextLexeme)
import Utils (colorSubstringWithRed)
import Data.Char (isSpace)

type EitherTokenError = Either LexicalError Token

scanner :: String -> Either String [Token]
scanner sourceCode = handleLexicalErrors (scanTokens sourceCode 0 []) sourceCode

handleLexicalErrors :: Either LexicalError [Token] -> String -> Either String [Token]
handleLexicalErrors (Right tokens) _ = Right tokens
handleLexicalErrors (Left error) source = Left (generateErrorMessage error source) 

generateErrorMessage :: LexicalError -> String -> String
generateErrorMessage (start, end) source =  
  "Lexical error at " ++ show start ++ ": " ++ colorSubstringWithRed start end source

scanTokens :: String -> Int -> [Token] -> Either LexicalError [Token]
scanTokens [] _ tokens = Right tokens
scanTokens (x:xs) index tokens
  | isSpace x = scanTokens xs (index+1) tokens
  | otherwise = keepScanning (scanNextLexeme (x:xs) index) tokens (x:xs)

keepScanning :: Either LexicalError Token -> [Token] -> String -> Either LexicalError [Token]
keepScanning (Left error) _ _ = Left error 
keepScanning (Right (Token lexeme (start, end))) tokens str = scanTokens (drop (1 + end - start) str) (end+1) (tokens ++ [Token lexeme (start, end)]) 
