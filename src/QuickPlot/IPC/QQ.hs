{-# LANGUAGE TemplateHaskell #-}

module QuickPlot.IPC.QQ (
      json
    , plotly
    , PlotlyJSON (..)
) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.Quote
import qualified Data.Vector as V
import qualified Data.Text as T
import           Data.Aeson hiding (parseJSON, json)
import           QuickPlot.IPC.QQParser


json :: QuasiQuoter
json = QuasiQuoter { quoteExp  = jsonExp
                   , quotePat  = const $ error "No quotePat defined for jsonQQ"
                   , quoteType = const $ error "No quoteType defined for jsonQQ"
                   , quoteDec  = const $ error "No quoteDec defined for jsonQQ"
                   }

jsonExp :: String -> ExpQ
jsonExp string =
    case parseJSON string of
        Left err  -> error $ "JSON is invalid: " ++ show err
        Right val -> [| val |]


instance Lift JSONValue where
    lift (JSONString string)  = [| String (T.pack string) |]
    lift JSONNull             = [| Null |]
    lift (JSONObject objects) = [| object $jsonList |]
        where
              jsonList :: ExpQ
              jsonList = ListE <$> mapM objs2list objects
              objs2list :: (HashKey, JSONValue) -> ExpQ
              objs2list (key, value) = case key of
                                        HashStringKey k -> [|(T.pack k, $(lift value))|]
                                        HashVarKey k    -> [|(T.pack $(dyn k), $(lift value))|]
    lift (JSONArray arr) = [| Array $ V.fromList $(ListE <$> mapM lift arr) |]
    lift (JSONNumber n)  = [| Number (fromRational $(return $ LitE $ RationalL (toRational n))) |]
    lift (JSONBool b)    = [| Bool b |]
    lift (JSONCode e)    = [| toJSON $(return e) |]



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
    case parseJSON string of
        Left err  -> error $ "JSON is invalid: " ++ show err
        Right val -> [| PlotlyJSON val |]


data PlotlyJSON = PlotlyJSON Value

instance ToJSON PlotlyJSON where
    toJSON (PlotlyJSON value) = value

instance Lift PlotlyJSON where
    lift (PlotlyJSON val) = [| PlotlyJSON val |]

instance Lift Value where
    lift value = [| value |]
