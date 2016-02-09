-- TODO: If browser refreshes the websocket connection doesn't close
--          then 2 threads read from the channel and only every second message arrives

{-# LANGUAGE OverloadedStrings #-}

module QuickPlot.IPC.Server (
    runServer,
    sendMessage
) where

import Network.WebSockets.Snap
import Network.WebSockets hiding (runServer)
import Data.ByteString.Lazy.Internal
import Snap
import Snap.Util.FileServe
import Control.Concurrent
import System.IO.Unsafe
import Control.Exception
import Control.Monad
import QuickPlot.IPC.Protocol


-- | Run snap server in background as a new process
-- Start only once (even after reloading in ghci)
-- Serves at "http://localhost:8000"
runServer :: FilePath -- ^ Path to directory that contains user scripts
          -> Int      -- ^ Port of the server
          -> IO ()
runServer userDir port = do
    _ <- forkIO $ httpServe config (service "src/frontend")
    return ()
    where config = setErrorLog ConfigNoLog $
                   setAccessLog ConfigNoLog $
                   setPort port
                   defaultConfig


-- | Declare the routing and the services of the server
service :: FilePath -- ^ Path to directory of files to serve
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
sendRawMessage :: ByteString
               -> IO ()
sendRawMessage = putMVar channel


-- | Send a message to the browser
-- QuickPlot server must be running otherwise undefined behaviour
sendMessage :: QPMessage
            -> IO ()
sendMessage = sendRawMessage . encode
