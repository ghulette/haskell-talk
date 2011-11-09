import IO
import Data.Char

-- pureFunction :: Int -> Int
-- pureFunction n = do
--   print n
--   return (n + 1)

countWordsInFile :: FilePath -> IO Int
countWordsInFile f = do
  h <- openFile f ReadMode
  s <- hGetContents h
  let ws = words s
  return (length ws)

main :: IO ()
main = do
  n <- countWordsInFile "fib.hs"
  let msg = "I counted " ++ (show n) ++ " words"
  putStrLn msg
