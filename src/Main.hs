import System.Environment
import Download
import ProcessPage

main = do
  args <- getArgs
  putStrLn ("So! Your args are " ++ show args)
  page <- download (head args)
  print (processPage page)
