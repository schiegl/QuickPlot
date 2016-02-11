{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module QuickPlot.Plotly (
      plotly
    , PlotlyJSON (..)
) where

import QuickPlot
import QuickPlot.IPC.Protocol
import QuickPlot.IPC.QQParser
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Aeson hiding (json)


instance Plottable PlotlyJSON where
    plottableToJSON (PlotlyJSON trace) = [json|{
                                    data : [ #{ trace } ]
                                }|]
    whichLibrary _ = Plotly

instance Plottable [PlotlyJSON] where
    plottableToJSON traces = [json|{
                        data : #{ traces }
                    }|]
    whichLibrary _ = Plotly

instance Plottable (PlotlyJSON, PlotlyJSON) where
    plottableToJSON (traces, layout) = [json|{
                                  data : [ #{ traces } ]
                                , layout : #{ layout }
                              }|]
    whichLibrary _ = Plotly

instance Plottable ([PlotlyJSON], PlotlyJSON) where
    plottableToJSON (traces, layout) = [json|{
                                  data : #{ traces }
                                , layout : #{ layout }
                              }|]
    whichLibrary _ = Plotly


-- This quasiquoter is the same as json but it wraps the value with PlotlyJSON
-- It makes writing code more clear when using multiple libraries
plotly :: QuasiQuoter
plotly = QuasiQuoter { quoteExp  = plotlyExp
                     , quotePat  = const $ error "No quotePat defined for jsonQQ"
                     , quoteType = const $ error "No quoteType defined for jsonQQ"
                     , quoteDec  = const $ error "No quoteDec defined for jsonQQ"
                     }

plotlyExp :: String -> ExpQ
plotlyExp string =
    case parseTHJSON string of
        Left errorInfo  -> error $ "JSON is invalid: " ++ show errorInfo
        Right value     -> [| PlotlyJSON value |]


data PlotlyJSON = PlotlyJSON Value

instance ToJSON PlotlyJSON where
    toJSON (PlotlyJSON value) = value

instance Lift PlotlyJSON where
    lift (PlotlyJSON value) = [| PlotlyJSON value |]
