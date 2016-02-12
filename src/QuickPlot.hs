{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module QuickPlot (
      module QuickPlot.IPC.QQ
    , runQuickPlot
    , runQuickPlotWith
    , Plottable (..)
    , plot
    , clear
    , toJSON
) where

import QuickPlot.IPC.Server
import QuickPlot.IPC.QQ
import Data.Aeson hiding (json)
import Data.Vector hiding ((++))
import QuickPlot.IPC.Protocol


-- | Port the QuickPlot server is supposed to use
type Port = Int
-- | Directory path of the QuickPlot client files
type UserDirectory = FilePath


-- TODO: Load scripts from custom directory into the real index.html
-- | Start a QuickPlot server
-- Run this function only once in a ghci session (even after reload)
runQuickPlotWith :: UserDirectory
                 -> Port
                 -> IO ()
runQuickPlotWith = runServer

-- | Start a QuickPlot server at "http://localhost:8000"
-- Run this function only once in a ghci session (even after reload)
runQuickPlot :: IO ()
runQuickPlot = runQuickPlotWith "" 8000



-- | Remove all plots in the browser
-- If the browser is not connected by now the behaviour is undefined
clear :: IO ()
clear = sendMessage (QPMessage QuickPlot Clear Null)


-- | Show plottable data on the browser via plotly
-- If the browser is not connected by now the behaviour is undefined
plot :: (Plottable p)
     => p
     -> IO ()
plot content = sendMessage (QPMessage (whichLibrary content) NewPlot (plottableToJSON content))


-- All the instances of the class are plottable and should be encoded in a JSON structure
-- that the library can process
class Plottable a where
    plottableToJSON :: a -> Value
    whichLibrary :: a -> Library
