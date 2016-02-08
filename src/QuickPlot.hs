-- TODO: If browser refreshes the websocket connection doesn't close
--          then 2 threads read from the channel and only every second message arrives

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickPlot (
    runQuickPlot,
    runQuickPlotWith,
    sendMessage
) where

import Network.WebSockets.Snap
import Network.WebSockets
import Data.ByteString.Lazy.Internal
import Snap
import Snap.Util.FileServe
import Control.Concurrent
import System.IO.Unsafe
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.QQ


-- | Run default QuickPlot server in background as a new process
-- Start only once (even after reloading in ghci)
-- Serves at "http://localhost:8000"
runQuickPlot :: IO ()
runQuickPlot = runQuickPlotWith "src/frontend"


-- | Run QuickPlot server in background as a new process
-- Start only once (even after reloading in ghci)
-- Serves at "http://localhost:8000"
runQuickPlotWith :: FilePath -- Path to directory of files to serve
                 -> IO ()
runQuickPlotWith staticDir = do
    _ <- forkIO $ httpServe config (service staticDir)
    return ()
    where config = setErrorLog ConfigNoLog $
                   setAccessLog ConfigNoLog
                   defaultConfig


-- | Declare the routing and the services of the server
service :: FilePath -- Path to directory of files to serve
        -> Snap ()
service staticDir = route [ ("/", serveDirectory staticDir)
                          , ("/ws", runWebSocketsSnap wsHandler)
                          , ("", serveFile "src/404.html")
                          ]


-- | Handle connection exceptions of the websocket
-- TODO: Clean up instead of just messaging
close :: ConnectionException
      -> IO ()
close (CloseRequest _ _) = print "Exception: CloseRequest"
close ConnectionClosed   = print "Exception: ConnectionClosed"
close exception          = print $ "Exception: " ++ show exception


-- | Handle new websocket connections
-- Send content of channel to browser and clear channel afterwards
wsHandler :: PendingConnection
          -> IO ()
wsHandler pending = do
    connection <- acceptRequest pending
    forkPingThread connection 20 -- keep alive (every 30 seconds)
    print "Browser connected" -- DEBUG
    handle close $ forever $ do
        msg <- takeMVar channel
        sendTextData connection msg
        threadDelay 500000


-- | Contains messages for the browser
channel :: MVar ByteString
{-# NOINLINE channel #-}
channel = unsafePerformIO newEmptyMVar


-- | Send a raw message to the browser
-- By setting the mvar which the server reads from for new messages
sendRawMessage :: ByteString -> IO ()
sendRawMessage = putMVar channel


-- | Send a message to the browser
-- The browser should already be connected otherwise nothing will happen
sendMessage :: (ToJSON json)
            => String -- Library name
            -> String -- Procedure name
            -> json   -- Message content
            -> IO ()
sendMessage library procedure content = sendRawMessage (encode value)
    where value = [aesonQQ|
                    {
                        library   : #{ library   },
                        procedure : #{ procedure },
                        content   : #{ content   }
                    }
                  |]
