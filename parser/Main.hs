module Main where
import Parser
import System.Environment(getArgs)

main :: IO ()
main = do
    args <- getArgs
    if length args < 2 then
      putStrLn "Error:Params:too few arguments"
    else do
      file <- readFile (args !! 0)
      let output = runParse file
      writeFile (args !! 1) output
      putStrLn "OK"
