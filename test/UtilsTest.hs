module UtilsTest 
(utilsTest)
where 

import Test.QuickCheck
import Utils (substring, substringInclusive, tripleFst, tripleSnd, tripleThrd)
import Data.List (isSubsequenceOf)

utilsTest :: IO ()
utilsTest = do 
              putStrLn "Testing Utils..."
              quickCheck prop_substringlength
              quickCheck prop_substringIsSubsequence
              quickCheck prop_substringInclusiveLength
              quickCheck prop_substringInclusiveIsSubsequence
              quickCheck prop_firstMemberOfTriple
              quickCheck prop_secondMemberOfTriple
              quickCheck prop_thirdMemberOfTriple

prop_substringIsSubsequence :: String -> Int -> Int  -> Property
prop_substringIsSubsequence str start end = (start >= 0 && start <= end) ==> isSubsequenceOf (substring str start end) str

prop_substringlength :: String -> Int -> Int  -> Property
prop_substringlength str start end =  (start >= 0 && start <= end && end <= length str) ==> length (substring str start end) === end - start

prop_substringInclusiveLength :: String -> Int -> Int  -> Property
prop_substringInclusiveLength str start end = (start >= 0 && start <= end && end <= length str) ==> 
    length (substring str start end) == length (substringInclusive str start end) || length (substringInclusive str start end) == length (substring str start end) + 1

prop_substringInclusiveIsSubsequence :: String -> Int -> Int  -> Property
prop_substringInclusiveIsSubsequence str start end = (start >= 0 && start <= end) ==> isSubsequenceOf (substringInclusive str start end) str

prop_firstMemberOfTriple :: Int -> Int -> Int -> Bool
prop_firstMemberOfTriple x y z = tripleFst (x, y, z) == x

prop_secondMemberOfTriple :: Int -> Int -> Int -> Bool
prop_secondMemberOfTriple x y z = tripleSnd (x, y, z) == y

prop_thirdMemberOfTriple :: Int -> Int -> Int -> Bool
prop_thirdMemberOfTriple x y z = tripleThrd (x, y, z) == z