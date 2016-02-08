-- TODO: Check with license of author

{-# LANGUAGE TemplateHaskell #-}

module QuickPlot.QQ (
    json
) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import qualified Data.Vector as V
import qualified Data.Text as T
import           Data.Aeson hiding (parseJSON, json)
import           QuickPlot.QQParser

json :: QuasiQuoter
json = QuasiQuoter { quoteExp  = jsonExp
                   , quotePat  = const $ error "No quotePat defined for jsonQQ"
                   , quoteType = const $ error "No quoteType defined for jsonQQ"
                   , quoteDec  = const $ error "No quoteDec defined for jsonQQ"
                   }


jsonExp :: String -> ExpQ
jsonExp string =
    case parsed of
        Left err  -> error $ "Error in aesonExp: " ++ show err
        Right val -> toExp val
    where parsed = parseJSON string

----
-- JSValue etc to ExpQ
---------
toExp :: JsonValue -> ExpQ
toExp (JsonString str)  = [|String (T.pack str)|]
toExp  JsonNull         = [|Null|]
toExp (JsonObject objs) = [|object $jsList|]
    where
        jsList :: ExpQ
        jsList = ListE <$> mapM objs2list objs
        objs2list :: (HashKey, JsonValue) -> ExpQ
        objs2list (key, value) = case key of
                                    HashStringKey k -> [|(T.pack k, $(toExp value))|]
                                    HashVarKey k    -> [|(T.pack $(dyn k), $(toExp value))|]

toExp (JsonArray arr) = [|Array $ V.fromList $(ListE <$> mapM toExp arr)|]
toExp (JsonNumber n)  = [|Number (fromRational $(return $ LitE $ RationalL (toRational n)))|]
toExp (JsonBool b)    = [|Bool b|]
toExp (JsonCode e)    = [|toJSON $(return e)|]
