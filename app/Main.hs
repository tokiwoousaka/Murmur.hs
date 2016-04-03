{-# LANGUAGE DataKinds #-}
module Main where
import Web.Twitter.Conduit
import Web.Twitter.Murmur 
import Control.Monad.IO.Class
import Options.Declarative
import System.Directory

main :: IO ()
main = run_ $ Group "Simple twitter CLI client"
  [ subCmd "home" cmdHome
  , subCmd "post" cmdPost
  , subCmd "mentions" cmdMention
  , subCmd "reply" cmdReply
  , subCmd "auth" cmdAuth
  ]
  
cmdHome :: Cmd "Show your home timeline" ()
cmdHome = liftIO $ do
  twInfo <- readTWInfo
  showHomeTimeline twInfo

cmdPost :: Arg "MESSAGE" String -> Cmd "Post new tweet" ()
cmdPost msg = liftIO $ do
  twInfo <- readTWInfo
  postTweet twInfo $ get msg

cmdMention :: Cmd "Show mentions for you" ()
cmdMention = liftIO $ do
  twInfo <- readTWInfo
  showMentionsTimeline twInfo  

cmdReply :: Flag "i" '["Id"] "StatusID" "Target tweet id" Integer 
  -> Arg "MESSAGE" String -> Cmd "Post new tweet" ()
cmdReply id msg = liftIO $ do
  twInfo <- readTWInfo
  postWithReplyId twInfo (get msg) $ get id

cmdAuth :: Cmd "Authentication twitter account" ()
cmdAuth = liftIO $ do
  twInfo <- getTWInfo murAuth
  twInfoFileName >>= flip writeFile (show twInfo)

----

readTWInfo :: IO TWInfo
readTWInfo = twInfoFileName >>= readFile >>= return . read

twInfoFileName :: IO String
twInfoFileName = do
  homeDir <- getHomeDirectory
  createDirectoryIfMissing False $ homeDir ++ "/.murmur"
  return $ homeDir ++ "/.murmur/twInfo"

----

murAuth :: MurmurAuth 
murAuth = MurmurAuth
  { murConsumerKey= consumerKey
  , murConsumerSecret = consumerSecret
  , murGetPINAction = \url -> do
      putStrLn $ "Access '" ++ url ++ "' for Accept. And type PIN code here."
      getLine
  }

-- 俺はおまいらがコレを悪用したりしないって信じてる
consumerKey :: String
consumerKey = "KfczaBGJTRNFT7NdhT4DJg"

consumerSecret :: String
consumerSecret = "HpExCp1ndUyHC3TzEyTtGZHSfYlo3BgP0kr2t3VtI8"

