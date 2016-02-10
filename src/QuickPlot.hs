{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module QuickPlot (
      module QuickPlot.IPC.QQ
    , runQuickPlot
    , runQuickPlotWith
    , Plottable (..)
    , plot
    , clear
) where

import QuickPlot.IPC.Server
import QuickPlot.IPC.QQ
import Data.Aeson hiding (toJSON, json)
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
runQuickPlotWith userDir port = do
    putStrLn $ mconcat ["Find QuickPlot at \"localhost:", show port, "\" in your browser"]
    runServer userDir port


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
plot content = sendMessage (QPMessage (getLibrary content) NewPlot (toJSON content))



-- The reason why I didn't just use ToJSON is because it is defined for values that plotly can't interpret
-- I kept the name "toJSON" though for clarity reasons and assume that people will use the quasi-quotes
-- and don't parse JSON themselves. But I might change my mind.

class Plottable a where
    toJSON :: a -> Value
    getLibrary :: a -> Library

instance (Num x, ToJSON x) => Plottable [x] where
    toJSON xs = [json| { data : [{ x : #{ xs } }] } |]
    getLibrary xs = Plotly

instance (Num x, ToJSON x) => Plottable (Vector x) where
    toJSON xs = [json| { data : [{ x : #{ xs } }] } |]
    getLibrary xs = Plotly

instance (Num x, ToJSON x, Num y, ToJSON y) => Plottable ([x],[y]) where
    toJSON (xs, ys) = [json| { data : [{ x : #{ xs }, y : #{ ys } }] } |]
    getLibrary (xs, ys) = Plotly

instance (Num x, ToJSON x, Num y, ToJSON y) => Plottable (Vector x, Vector y) where
    toJSON (xs, ys) = [json| { data : [{ x : #{ xs }, y : #{ ys } }] } |]
    getLibrary (xs, ys) = Plotly

-- TODO: To: Future me or stranger
--       Message: Think of a way to not have JSON values by default plotted by plotly
instance Plottable Value where
    toJSON v = [json| { data : [ #{ v } ] } |]
    getLibrary v = Plotly

instance Plottable [Value] where
    toJSON v = [json| { data : #{ v } } |]
    getLibrary v = Plotly
