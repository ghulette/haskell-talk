import Network.HTTP
import Control.Monad

domains = ["google.com","yahoo.com","apple.com"]

readUrl :: String -> IO String
readUrl url = do
  result <- simpleHTTP (getRequest url)
  resp <- getResponseBody result
  return resp

main :: IO ()
main = do
  let urls = map ("http://www." ++) domains
  let urlWordCount = liftM (length . words) . readUrl
  counts <- mapM urlWordCount urls
  mapM_ print counts
  