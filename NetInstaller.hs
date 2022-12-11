import System.Environment
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit.Combinators     (sinkFile)
import Network.HTTP.Conduit         (parseRequest)
import Network.HTTP.Simple          (httpSink)


downloadFile :: String -> String -> IO ()
downloadFile url dst = do
  request <- parseRequest url
  runResourceT $ httpSink request $ (\_ -> sinkFile dst)

main :: IO ()
main = do
    args <- getArgs
    let url = head args
    let dst = head (tail args)
    downloadFile url dst
