{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickPlot.Vis (
      vis
    , VisJSON (..)
    , VisPlotType (..)
    , VisData
    , VisOptions
) where


import QuickPlot
import QuickPlot.IPC.Protocol
import QuickPlot.IPC.QQParser
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Aeson hiding (json)
import Data.Text




type VisData = VisJSON
type VisOptions = VisJSON

-- | Only Network graphs work
data VisPlotType = Network
                 deriving Show


instance Plottable (VisPlotType, VisData) where
    plottableToJSON (plotType, (VisJSON trace)) = [json|{
                                      plotType : #{ plotType }
                                    , data : #{ trace }
                                    , options : {}
                             }|]
    whichLibrary _ = Vis

instance Plottable (VisPlotType, [VisData]) where
    plottableToJSON (plotType, traces) = [json|{
                          plotType : #{ plotType }
                        , data : #{ traces }
                        , options : {}
                    }|]
    whichLibrary _ = Vis

instance Plottable (VisPlotType, VisData, VisOptions) where
    plottableToJSON (plotType, traces, options) = [json|{
                                  plotType : #{ plotType }
                                , data : #{ traces }
                                , options : #{ options }
                              }|]
    whichLibrary _ = Vis

instance Plottable (VisPlotType, [VisData], VisOptions) where
    plottableToJSON (plotType, traces, options) = [json|{
                                  plotType : #{ plotType }
                                , data : #{ traces }
                                , options : #{ options }
                              }|]
    whichLibrary _ = Vis




-- This quasiquoter is the same as json but it wraps the value with VisJSON
-- It makes writing code more clear when using multiple libraries
vis :: QuasiQuoter
vis = QuasiQuoter { quoteExp  = visExp
                  , quotePat  = const $ error "No quotePat defined for visQQ"
                  , quoteType = const $ error "No quoteType defined for visQQ"
                  , quoteDec  = const $ error "No quoteDec defined for visQQ"
                  }


visExp :: String -> ExpQ
visExp string =
    case parseTHJSON string of
        Left errorInfo -> error $ "JSON is invalid: " ++ show errorInfo
        Right value    -> [| VisJSON value |]


data VisJSON = VisJSON Value


instance ToJSON VisJSON where
    toJSON (VisJSON value) = value

instance ToJSON VisPlotType where
    toJSON Network  = String "network"
    toJSON Timeline = String "timeline"
    toJSON Graph2D  = String "graph2d"
    toJSON Graph3D  = String "graph3d"

instance Lift VisJSON where
    lift (VisJSON value) = [| VisJSON value |]
