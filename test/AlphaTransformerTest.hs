module AlphaTransformerTest 
(testAlphaTransformer)
where 

import Test.QuickCheck
import Lexeme (Lexeme (..), Token(..))
import Grammar (Term (..), App(App), App'(..), Exp(..), stringifyTerm)
import AlphaTransformer (alphaTransform, alphaTransformExp, alphaTransformApp, alphaTransformApp', addIndex, bindVariable, getIndexedVariable)
import Data.Map (Map)
import qualified Data.Map as Map


testAlphaTransformer :: IO ()
testAlphaTransformer = 
    do
        putStrLn "Testing alphaTransformExp"
        quickCheck alphaTransformExp1
        quickCheck alphaTransformExp2
        quickCheck alphaTransformApp'1
        quickCheck alphaTransformApp'2
        quickCheck alphaTransformApp1
        quickCheck addIndexToEmptyIndex
        quickCheck addIndexToNonEmptyIndexMap
        quickCheck bindFreeVariable
        quickCheck bindBoundVariable
        quickCheck getIndexedVariableForIndexed
        quickCheck getIndexedVariableForNotIndexed
        quickCheck transformUnindexedLambda
        quickCheck transformIndexedLambda
        quickCheck transformMultipleLambda
        quickCheck unboundVariableIsNotTransformed

alphaTransformExp1 :: Property
alphaTransformExp1 = alphaTransformExp (Grammar.Var (Lexeme.Variable "x"), Map.singleton "x" 1, ["x"]) 
    === (Grammar.Var (Lexeme.Variable "x1"), Map.singleton "x" 1 )

-- unbound variable is not changed
alphaTransformExp2 :: Property
alphaTransformExp2 = alphaTransformExp (Grammar.Var (Lexeme.Variable "x"), Map.singleton "x" 1, []) 
    === (Grammar.Var (Lexeme.Variable "x"), Map.singleton "x" 1 )

alphaTransformApp'1 :: Property
alphaTransformApp'1 = alphaTransformApp' (Grammar.EMPTY, Map.singleton "x" 1, ["x"]) 
    === (Grammar.EMPTY, Map.singleton "x" 1)

alphaTransformApp'2 :: Property
alphaTransformApp'2 = alphaTransformApp' (Grammar.App' (Grammar.Var (Lexeme.Variable "x")) Grammar.EMPTY, Map.singleton "x" 1, ["x"]) 
    === (Grammar.App' (Grammar.Var (Lexeme.Variable "x1")) Grammar.EMPTY, Map.singleton "x" 1 )

alphaTransformApp1 :: Property
alphaTransformApp1 = alphaTransformApp (Grammar.App (Grammar.Var (Lexeme.Variable "x")) (Grammar.App' (Grammar.Var (Lexeme.Variable "x")) Grammar.EMPTY), Map.singleton "x" 1, ["x"]) 
    === (Grammar.App (Grammar.Var (Lexeme.Variable "x1")) (Grammar.App' (Grammar.Var (Lexeme.Variable "x1")) Grammar.EMPTY), Map.singleton "x" 1 )

addIndexToEmptyIndex :: Property
addIndexToEmptyIndex = Map.toList (addIndex Map.empty "x") === [("x", 1)]

addIndexToNonEmptyIndexMap :: Property
addIndexToNonEmptyIndexMap = Map.toList (addIndex (Map.singleton "x" 1) "x") === [("x", 2)]

bindFreeVariable :: Property
bindFreeVariable = bindVariable [] "x" === ["x"] 

bindBoundVariable :: Property
bindBoundVariable = bindVariable ["x"] "x" === ["x"] 

getIndexedVariableForIndexed :: Property
getIndexedVariableForIndexed = getIndexedVariable (Map.singleton "x" 1) "x" === "x1"

getIndexedVariableForNotIndexed :: Property
getIndexedVariableForNotIndexed = getIndexedVariable (Map.singleton "y" 1) "x" === "x"

transformUnindexedLambda :: Property
transformUnindexedLambda = alphaTransform (Abs (Lambda "x") (TermApp (App (Grammar.Var (Lexeme.Variable "x")) EMPTY)), Map.empty, []) 
 === (Abs (Lambda "x1") (TermApp (App (Grammar.Var (Lexeme.Variable "x1")) EMPTY)), Map.singleton "x" 1)

transformIndexedLambda :: Property
transformIndexedLambda = alphaTransform (Abs (Lambda "x") (TermApp (App (Grammar.Var (Lexeme.Variable "x")) EMPTY)), Map.singleton "x" 1, []) 
 === (Abs (Lambda "x2") (TermApp (App (Grammar.Var (Lexeme.Variable "x2")) EMPTY)), Map.singleton "x" 2)

transformMultipleLambda :: Property
transformMultipleLambda = alphaTransform (Abs (Lambda "x") (Abs (Lambda "x") (TermApp (App (Grammar.Var (Lexeme.Variable "x")) EMPTY))), Map.empty, []) 
 === (Abs (Lambda "x1") (Abs (Lambda "x2") (TermApp (App (Grammar.Var (Lexeme.Variable "x2")) EMPTY))), Map.singleton "x" 2)

unboundVariableIsNotTransformed :: Property
unboundVariableIsNotTransformed = alphaTransform (Abs (Lambda "x") (TermApp (App (Grammar.Var (Lexeme.Variable "x")) (App' (Grammar.Var (Lexeme.Variable "y")) EMPTY))), Map.empty, []) 
 === (Abs (Lambda "x1") (TermApp (App (Grammar.Var (Lexeme.Variable "x1")) (App' (Grammar.Var (Lexeme.Variable "y")) EMPTY))), Map.singleton "x" 1)