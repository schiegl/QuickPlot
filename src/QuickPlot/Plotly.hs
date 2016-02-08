{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module QuickPlot.Plotly (
       plot
     , clear
     , PlotlyTrace (..)
     , PlotlyMode (..)
     , scatter
     , histogram
) where

import Prelude hiding (null)
import Data.Aeson.QQ
import Data.Aeson
import Data.ByteString.Lazy.Internal
-- import Data.Colour.SRGB
-- import Data.Colour.Names
import QuickPlot
import qualified Data.List as L
import Data.Vector.Unboxed


plot :: (ToJSON a) => [a] -> IO ()
plot traces = sendMessage "plotly" "newPlot" json
    where json = [aesonQQ|
                    {
                        data : #{ traces }
                    }
                 |]


data PlotlyMode = Lines
                | Markers
                | Text

instance Show PlotlyMode where
    show Markers = "markers"
    show Lines   = "lines"
    show Text    = "text"

data PlotlyTrace =
      ScatterTrace   { x :: Vector Double
                     , y :: Vector Double
                     , mode :: [PlotlyMode]
                     , name :: String
                     }
    | HistogramTrace { x :: Vector Double
                     , name :: String
                     , opacity :: Double
                     }

scatter = ScatterTrace { x    = empty
                       , y    = empty
                       , mode = [Markers]
                       , name = ""
                       }

histogram = HistogramTrace { x       = empty
                           , name    = ""
                           , opacity = 1
                           }

-- HACK: Instead of skipping values set them to something plotly ignores
instance ToJSON PlotlyTrace where
    toJSON (ScatterTrace x y modes name) = json
        where x'     = if null x then "ignore" else "x"
              y'     = if null y then "ignore" else "y"
              mode'  = if L.null modes then "ignore" else "mode"
              name'  = if L.null name then "ignore" else "name"
              json   = [aesonQQ|
                        {
                            type   : "scatter",
                            $x'    : #{ x },
                            $y'    : #{ y },
                            $mode' : #{ L.intercalate "+" $ fmap show modes },
                            $name' : #{ name }
                        }
                    |]
    toJSON (HistogramTrace x name opacity) = json
        where x'    = if null x then "ignore" else "x"
              name' = if L.null name then "ignore" else "name"
              json  = [aesonQQ|
                        {
                            type    : "histogram",
                            $x'     : #{ x },
                            $name'  : #{ name },
                            opacity : #{ opacity }
                        }
                    |]



-- | Remove all plots in the browser
clear :: IO ()
clear = sendMessage "general" "clear" ""
