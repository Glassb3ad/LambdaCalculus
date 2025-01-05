module Utils 
(substring,
substringInclusive,
join,
colorSubstringWithRed,
tripleFst,
tripleSnd,
tripleThrd)
where 

substring :: String -> Int -> Int -> String
substring str start end =  take (end-start) (drop start str)

substringInclusive :: String -> Int -> Int -> String
substringInclusive str start end =  substring str start (end+1)

colorSubstringWithRed :: Int -> Int -> String -> String
colorSubstringWithRed start end str = take start str  ++ "\x1b[91m" ++ substringInclusive str start end ++ "\x1b[0m" ++ drop (end+1) str

join :: [String] -> String
join = foldl (++) ""

tripleFst :: (a, b, c) -> a
tripleFst (a, _, _) = a

tripleSnd :: (a, b, c) -> b
tripleSnd (_, b, _) = b

tripleThrd :: (a, b, c) -> c
tripleThrd (_, _, c) = c