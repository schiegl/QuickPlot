{-
    A little snap server that lives at localhost that
        - communicates over websockets at "localhost:PORT/ws"
        - answers HTTP requests on any other localhost URL combination
-}

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
import Data.IORef



-- | Run snap server in background as a new process
-- Run only once (even after reloading in ghci)
runServer :: FilePath -- ^ Path to directory that contains user scripts
          -> Int      -- ^ Port of the server
          -> IO ()
runServer userDir port = do
    running <- readIORef serverRunning
    if running
        then putStrLn "Start QuickPlot server only once per ghci session"
        else do
            putStrLn $ mconcat ["Find QuickPlot at \"localhost:", show port, "\" in your browser"]
            atomicWriteIORef serverRunning True
            _ <- forkIO $ httpServe config (service "src/frontend")
            void $ takeMVar channel -- HACK: Reading "stop" before websocketHandler so it doesn't block
    where config =   setErrorLog ConfigNoLog
                   $ setAccessLog ConfigNoLog
                   $ setVerbose False
                   $ setPort port
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
                     -> IO ()      -- ^ Message sent to browser
handleUntilNewClient connection = do
    msg <- takeMVar channel
    case msg of
        "stop" -> return ()
        _      -> do sendTextData connection msg
                     handleUntilNewClient connection


-- | Handle connection exceptions of the websocket
-- TODO: Clean up instead of just messaging
close :: ConnectionException -- ^ Websocket connection
      -> IO ()               -- ^ Message that exception happened in stdout
close (CloseRequest _ _) = print "Exception: CloseRequest"
close ConnectionClosed   = print "Exception: ConnectionClosed"
close exception          = print $ "Exception: " ++ show exception



-- | Contains the newest message for the browser
channel :: MVar ByteString -- ^ Message for the browser
{-# NOINLINE channel #-}
channel = unsafePerformIO newEmptyMVar


-- | Set to true if server is running
serverRunning :: IORef Bool -- ^ True if server running
{-# NOINLINE serverRunning #-}
serverRunning = unsafePerformIO (newIORef False)


-- | Send a raw message to the browser
-- By setting the channel which the server reads from for new messages
sendRawMessage :: ByteString -- ^ Message for the browser
               -> IO ()      -- ^ Either message sent to browser or reminder to start server in stdout
sendRawMessage message = do
    running <- readIORef serverRunning
    if not running
        then putStrLn "You need to start QuickPlot with \"runQuickPlot\" before you can plot"
        else putMVar channel message


-- | Send a message to the browser
sendMessage :: QPMessage -- ^ Message for the browser
            -> IO ()     -- ^ Either message sent to server or rminder to start server in stdout
sendMessage = sendRawMessage . encode
