import System.Environment
import System.IO

main = do
    (inFileName:outFileName:_) <- getArgs
    putStrLn inFileName
    putStrLn outFileName
