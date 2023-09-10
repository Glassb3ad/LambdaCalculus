module Grammar
(Term (Abs, TermApp), App(App), App'(App', EMPTY), Exp(Var, WithParenthesis), stringifyTerm)
where 

import Lexeme (Lexeme (..))
import Utils (join)

data Term = Abs Lexeme Term | TermApp App
    deriving ( Eq, Show )

data App = App Exp App'
    deriving ( Eq, Show )

data App' = App' Exp App' | EMPTY
    deriving ( Eq, Show )

data Exp = Var Lexeme | WithParenthesis Term 
    deriving ( Eq, Show )

stringifyTerm :: Term -> String
stringifyTerm = stringifyTermHelper ""  

stringifyTermHelper :: String -> Term -> String  
stringifyTermHelper str (Abs (Lambda var) term) = stringifyTermHelper (join [str, getLambda str, var, "."]) term
stringifyTermHelper str (TermApp app) = stringifyApp str app

stringifyApp :: String -> App -> String
stringifyApp str (App exp app') = stringifyApp' (stringifyExp str exp) app'

stringifyApp' :: String -> App' -> String
stringifyApp' str EMPTY = str
stringifyApp' str (App' exp app') = stringifyApp' (stringifyExp str exp) app'

stringifyExp :: String -> Exp -> String
stringifyExp str (Var (Variable var)) = getVar str var
stringifyExp str (WithParenthesis term) = join [str, getLeftPar str, stringifyTerm term,")"]

getLambda :: String -> String
getLambda [] = "λ"
getLambda str = if last str == ')' then " λ" else "λ"

getVar :: String -> String -> String
getVar [] var = var
getVar str var = join [str, " ",var]

getLeftPar :: String -> String
getLeftPar [] = "("
getLeftPar _ = " ("