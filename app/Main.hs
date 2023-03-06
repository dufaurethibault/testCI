import MarcelVM (run_program)

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure, ExitSuccess))

-- how to exit with a non-zero exit code?
-- exitWith $ ExitCode 1

main_exit :: Int -> IO ()
main_exit 0 = exitWith ExitSuccess
main_exit n = exitWith $ ExitFailure n

main' :: [String] -> IO Int
main' [file] = run_program file
main' _ = putStrLn "Usage: marcelvm <file>" >> return 1

main :: IO ()
main = getArgs >>= main' >>= main_exit