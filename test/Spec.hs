import ScannerTest (testScanner)
import UtilsTest (utilsTest)

main :: IO ()
main = do 
        putStrLn "Tests started"
        utilsTest
        testScanner
