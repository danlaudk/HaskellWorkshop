import System.Environment
import Download
import ProcessPage
import Spider
import Data.Map

main :: IO()
main = do 
  args <- getArgs
  theMap <- spider 5000 (head (args ++ ["http://www.ordina.nl"]))
  putStrLn (showTreeWith showItem True False theMap)
 where
   showItem k x = take 80 $ show k --(k,x) 

main1 :: IO()
main1 = do
  args <- getArgs
  putStrLn ("So! Your args are " ++ show args)
  page <- download (head args)
  output <- processPage page
  print output
