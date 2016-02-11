{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module QuickPlot.Vis (
      vis
    , VisJSON (..)
) where


import QuickPlot
import QuickPlot.IPC.Protocol
import QuickPlot.IPC.QQParser
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Aeson hiding (json)


instance Plottable VisJSON where
    plottableToJSON (VisJSON trace) = [json|{
                                      data : #{ trace }
                                    , options : {}
                             }|]
    whichLibrary _ = Vis

instance Plottable [VisJSON] where
    plottableToJSON traces = [json|{
                          data : #{ traces }
                        , options : {}
                    }|]
    whichLibrary _ = Vis

instance Plottable (VisJSON, VisJSON) where
    plottableToJSON (traces, options) = [json|{
                                  data : #{ traces }
                                , options : #{ options }
                              }|]
    whichLibrary _ = Vis

instance Plottable ([VisJSON], VisJSON) where
    plottableToJSON (traces, options) = [json|{
                                  data : #{ traces }
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

-- instance ToJSON [VisJSON] where
--     toJSON values = toJSON (fmap toJSON values)

instance Lift VisJSON where
    lift (VisJSON value) = [| VisJSON value |]
