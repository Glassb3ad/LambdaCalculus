module AlphaTransformer
(alphaTransform, alphaTransformExp, alphaTransformApp, alphaTransformApp', addIndex, bindVariable, getIndexedVariable)
where 

import Grammar (Term (Abs, TermApp), App(App), App'(App', EMPTY), Exp(Var, WithParenthesis))
import Lexeme (Lexeme (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bifunctor (first)
import Data.Maybe (isJust, fromJust)

getIndexedVariable :: Map String Int -> String -> String
getIndexedVariable indexMap variable = if isJust (Map.lookup variable indexMap)
    then variable ++ show (fromJust (Map.lookup variable indexMap))
    else variable

addIndex :: Map String Int -> String -> Map String Int
addIndex indexMap str = if isJust (Map.lookup str indexMap)
    then Map.insert str (fromJust (Map.lookup str indexMap) + 1) indexMap
    else Map.insert str 1 indexMap

bindVariable :: [String] -> String -> [String]
bindVariable bound variable = if variable `elem` bound
    then bound
    else variable : bound 

alphaTransform :: (Term, Map String Int, [String]) -> (Term, Map String Int)
alphaTransform (TermApp app, indexMap, bound) =
    let res = alphaTransformApp (app, indexMap, bound)
    in first TermApp res
alphaTransform (Abs (Lambda var) term, indexMap, bound) =
    let newIndexMap = addIndex indexMap var
        res = alphaTransform (term, newIndexMap, bindVariable bound var)
    in (Abs (Lambda (getIndexedVariable newIndexMap var)) (fst res), snd res)

alphaTransformApp :: (App, Map String Int, [String]) -> (App, Map String Int)
alphaTransformApp (App exp app', indexMap, bound) = 
    let expRes = alphaTransformExp (exp, indexMap, bound)
        appRes = alphaTransformApp' (app', snd expRes, bound)
    in  first (App (fst expRes)) appRes

alphaTransformApp' :: (App', Map String Int, [String]) -> (App', Map String Int)
alphaTransformApp' (EMPTY, indexMap, _) = (EMPTY, indexMap)  
alphaTransformApp' (App' exp app, indexMap, bound) = 
        let expRes = alphaTransformExp (exp, indexMap, bound)
            appRes = alphaTransformApp' (app, snd expRes, bound)
        in first (App' (fst expRes)) appRes  

alphaTransformExp :: (Exp, Map String Int, [String]) -> (Exp, Map String Int)
alphaTransformExp (Var (Lexeme.Variable variable), indexMap, bound) = 
    if variable `elem` bound
    then (Var (Lexeme.Variable (getIndexedVariable indexMap variable)), indexMap)
    else (Var (Lexeme.Variable variable), indexMap)
alphaTransformExp (WithParenthesis term, indexMap, bound) = 
    let res =  alphaTransform (term, indexMap, bound)
    in first WithParenthesis res