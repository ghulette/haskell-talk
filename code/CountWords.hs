import IO
import System (getArgs)
import Data.Char

countWordsInFile :: FilePath -> IO Int
countWordsInFile f = do
  h <- openFile f ReadMode
  s <- hGetContents h
  return ((length . words) s)

main :: IO ()
main = do
  [file] <- getArgs
  n <- countWordsInFile file
  let msg = "I counted " ++ (show n) ++ " words"
  putStrLn msg
