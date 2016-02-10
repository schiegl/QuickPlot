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
-- Run only once (even after reloading in ghci)
runServer :: FilePath -- ^ Path to directory that contains user scripts
          -> Int      -- ^ Port of the server
          -> IO ()
runServer userDir port = do
    _ <- forkIO $ httpServe config (service "src/frontend")
    _ <- takeMVar channel -- HACK: Reading "stop" before websocketHandler so it doesn't block
    return ()
    where config = setErrorLog ConfigNoLog $
                   setAccessLog ConfigNoLog $
                   setPort port
                   defaultConfig


-- | Declare the routing and the services of the server
service :: FilePath -- ^ Path to directory of files to serve
        -> Snap ()
service staticDir = route [ ("/", serveDirectory staticDir)
                          , ("/ws", runWebSocketsSnap websocketHandler)
                          , ("", serveFile "src/404.html")
                          ]


-- | Handle new websocket connections
-- Send content of channel to browser and clear channel afterwards
-- If the channel contains "stop" the next websocketHandler who reads it will stop
websocketHandler :: PendingConnection -- ^ About to be a websocket connection
                 -> IO ()
websocketHandler pending = do
    connection <- acceptRequest pending
    forkPingThread connection 20 -- keep alive (every 20 seconds)
    sendRawMessage "stop" -- HACK: runServer must read "stop" first, otherwise this connection halts
    handle close $ handleUntilNewClient connection


-- | Handle websocket connection until new client connects to server
-- Will halt if it reads "stop" in the channel
handleUntilNewClient :: Connection -- ^ Websocket connection
                     -> IO ()
handleUntilNewClient connection = do
    msg <- takeMVar channel
    case msg of
        "stop" -> return ()
        _     -> do sendTextData connection msg
                    threadDelay updateInterval
                    handleUntilNewClient connection


-- | Handle connection exceptions of the websocket
-- TODO: Clean up instead of just messaging
close :: ConnectionException -- ^ Websocket connection
      -> IO ()
close (CloseRequest _ _) = print "Exception: CloseRequest"
close ConnectionClosed   = print "Exception: ConnectionClosed"
close exception          = print $ "Exception: " ++ show exception



-- | Channel read interval of the server in microseconds
updateInterval :: Int
updateInterval = 500000


-- | Contains the newest message for the browser
channel :: MVar ByteString -- ^ Message for the browser
{-# NOINLINE channel #-}
channel = unsafePerformIO newEmptyMVar


-- | Send a raw message to the browser
-- By setting the channel which the server reads from for new messages
-- QuickPlot server must be running otherwise undefined behaviour
-- Should not be called faster than 0.5/s
sendRawMessage :: ByteString -- ^ Message for the browser
               -> IO ()
sendRawMessage = putMVar channel


-- | Send a message to the browser
-- QuickPlot server must be running otherwise undefined behaviour
-- Should not be called faster than 0.5/s
sendMessage :: QPMessage -- ^ Message for the browser
            -> IO ()
sendMessage = sendRawMessage . encode
