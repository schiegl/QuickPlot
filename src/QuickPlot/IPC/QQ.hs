{-# LANGUAGE TemplateHaskell #-}

module QuickPlot.IPC.QQ (
      json
) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.Quote
import qualified Data.Vector as V
import qualified Data.Text as T
import           Data.Aeson hiding (json)
import           QuickPlot.IPC.QQParser
--import           Data.Aeson.Types

json :: QuasiQuoter
json = QuasiQuoter { quoteExp  = jsonExp
                   , quotePat  = const $ error "No quotePat defined for jsonQQ"
                   , quoteType = const $ error "No quoteType defined for jsonQQ"
                   , quoteDec  = const $ error "No quoteDec defined for jsonQQ"
                   }

jsonExp :: String -> ExpQ
jsonExp string =
    case parseTHJSON string of
        Left err  -> error $ "JSON is invalid: " ++ show err
        Right val -> [| val |]


--instance Lift Value where
--    lift value = [| value |]


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
