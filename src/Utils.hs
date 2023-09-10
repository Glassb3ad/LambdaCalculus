module Utils 
(substring, substringInclusive, join, colorSubstringWithRed)
where 

substring :: String -> Int -> Int -> String
substring str start end =  take (end-start) (drop start str)

substringInclusive :: String -> Int -> Int -> String
substringInclusive str start end =  substring str start (end+1)

colorSubstringWithRed :: Int -> Int -> String -> String
colorSubstringWithRed start end str = take start str  ++ "\x1b[91m" ++ substringInclusive str start end ++ "\x1b[0m" ++ drop (end+1) str

join :: [String] -> String
join = foldl (++) ""