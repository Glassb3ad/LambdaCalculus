module Lexeme
(Lexeme (LeftParenthesis, RightParenthesis, Lambda, Variable), LexicalError, Token(Token), Position, scanNextLexeme)
where 

import Data.Char (isAlpha, isSpace)

data Token = Token Lexeme Position 
    deriving ( Eq, Show )
type Variable = String
type Position = (Int, Int) --Both indexes are Inclusive 
type LexicalError = Position
data Lexeme = LeftParenthesis | RightParenthesis | Lambda Variable| Variable Variable   
    deriving ( Eq, Show )

scanNextLexeme :: String -> Int -> Either LexicalError Token
scanNextLexeme ('(':xs) num = Right (Token LeftParenthesis (num,num))
scanNextLexeme (')':xs) num = Right (Token RightParenthesis (num,num))
scanNextLexeme ('λ':xs) num = scanLambda xs num []
scanNextLexeme str num = scanVariable str num []
scanNextLexeme [] num = Left (num,num)

scanVariable ::  String -> Int -> String -> Either LexicalError Token
scanVariable (x:xs) num res 
    | x `elem` [')', '(', 'λ'] || isSpace x = Right (Token (Variable res) (num, num + length res - 1))
    | isVariableChar x = scanVariable xs num (res ++ [x])                            
    | otherwise = Left (num, num + length res + 1)
scanVariable [] num res = Right (Token (Variable res) (num, num + length res - 1))

scanLambda :: String -> Int -> String -> Either LexicalError Token
scanLambda (x:xs) num res
    | x == '.' = Right (Token (Lambda res) (num, num + length res + 1))
    | isSpace x = Right (Token (Lambda res) (num, num + length res))
    | isVariableChar x = scanLambda xs num (res ++ [x])                            
    | otherwise = Left (num, num + length res + 1)
scanLambda [] num res = Right (Token (Lambda res) (num, num + length res))

isVariableChar :: Char -> Bool
isVariableChar = isAlpha