module Utils 
(substring, substringInclusive)
where 

substring :: String -> Int -> Int -> String
substring str start end =  take (end-start) (drop start str)

substringInclusive :: String -> Int -> Int -> String
substringInclusive str start end =  substring str start (end+1)
