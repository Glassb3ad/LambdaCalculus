import ScannerTest (testScanner)
import UtilsTest (utilsTest)
import ParserTest (testParser)

main :: IO ()
main = do 
        putStrLn "Tests started"
        putStrLn "Running utils tests"
        utilsTest
        putStrLn "Finished util tests \n"
        putStrLn "Running scanner tests"
        testScanner
        putStrLn "Finished scanner tests \n"
        putStrLn "Running parser tests"
        testParser
        putStrLn "Finished parser tests"

