-- TODO: Put current client's ThreadID into mvar and kill it once new one comes
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
    noClients <- isEmptyMVar clientThread
    if noClients
        then do
            atomicWriteIORef serverRunning (True, port)
            void $ forkIO $ httpServe config (service "src/frontend")
            putStrLn $ "Find QuickPlot at \"localhost:" ++ show port ++ "\" in your browser"

        else putStrLn "Start QuickPlot server only once per ghci session"

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
    login
    forkPingThread connection 20 -- keep alive (every 20 seconds)
    handle close $ handleUntilNewClient connection


-- | Handle websocket connection until new client connects to server
-- Will halt if it reads "stop" in the channel
handleUntilNewClient :: Connection -- ^ Websocket connection
                     -> IO ()      -- ^ Message sent to browser
handleUntilNewClient connection = do
    msg <- takeMVar channel
    sendTextData connection msg
    handleUntilNewClient connection


-- | Handle connection exceptions of the websocket
-- TODO: Clean up instead of just messaging
close :: ConnectionException -- ^ Websocket connection
      -> IO ()               -- ^ Message that exception happened in stdout
close (CloseRequest _ _) = print "Exception: CloseRequest"
close ConnectionClosed   = print "Exception: ConnectionClosed"
close exception          = print $ "Exception: " ++ show exception


-- | Login for client threads
-- It will kill other clients if there are any
login :: IO ()
login = do
    noClients <- isEmptyMVar clientThread
    if noClients
        then myThreadId >>= putMVar clientThread
        else do
            takeMVar clientThread >>= killThread
            myThreadId >>= putMVar clientThread



-- | Contains the newest message for the browser
channel :: MVar ByteString -- ^ Message for the browser
{-# NOINLINE channel #-}
channel = unsafePerformIO newEmptyMVar


-- | Tell if client connected and on which thread
clientThread :: MVar ThreadId -- ^ Thread of the current client if there is one
{-# NOINLINE clientThread #-}
clientThread = unsafePerformIO newEmptyMVar

-- | Tell if server was started and on which port
serverRunning :: IORef (Bool, Int) -- ^ (if running, port)
{-# NOINLINE serverRunning #-}
serverRunning = unsafePerformIO (newIORef (False, 0))


-- | Send a raw message to the browser
-- By setting the channel which the server reads from for new messages
sendRawMessage :: ByteString -- ^ Message for the browser
               -> IO ()      -- ^ Either message sent to browser or reminder to start server in stdout
sendRawMessage message = do
    print $ "Sending: " ++ show message
    noClients <- isEmptyMVar clientThread
    (running, port) <- readIORef serverRunning
    if running
        then if noClients
                then do
                    threadDelay 1000000 -- In case browser will reconnect itself
                    putStrLn $ "You need to go to: \"localhost:" ++ show port
                                    ++ "\" in your browser before plotting"
                else putMVar channel message
    else putStrLn "You need to start QuickPlot with \"runQuickPlot\" before plotting"


-- | Send a message to the browser
sendMessage :: QPMessage -- ^ Message for the browser
            -> IO ()     -- ^ Either message sent to server or rminder to start server in stdout
sendMessage = sendRawMessage . encode
