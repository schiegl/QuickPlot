{-
    The communication from the QuickPlot server to the browser will be in JSON
    Here is an example of how a message could look like:

        {
            "library"   : "plotly",
            "procedure" : "newPlot",
            "content"   : {
                            "data" : [ { "x" : [1,2,3] }, { "x" : [4,5,6] } ]
                          }
        }
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickPlot.IPC.Protocol (
      QPMessage (..)
    , Library (..)
    , Procedure (..)
    , encode

) where

import           Data.ByteString.Lazy.Internal
import           Data.Text
import           QuickPlot.IPC.QQ
import qualified Data.Aeson as JSON



-- | Encode a QuickPlot message for transmission to browser
encode :: QPMessage
       -> ByteString
encode (QPMessage library procedure content) = JSON.encode message
    where message :: JSON.Value
          message = [json|{
                        library : #{ library },
                        procedure : #{ procedure },
                        content : #{ content }
                    }|]


-- | Its possible to specify messages with procedures and libraries that don't exist together
data QPMessage = QPMessage Library Procedure JSON.Value

data Procedure = NewPlot
               | Clear
data Library = QuickPlot
             | Plotly
             | Vis

instance JSON.ToJSON Procedure where
    toJSON NewPlot = JSON.String "newPlot"
    toJSON Clear = JSON.String "clear"

instance JSON.ToJSON Library where
    toJSON QuickPlot = JSON.String "QuickPlot"
    toJSON Plotly = JSON.String "plotly"
    toJSON Vis = JSON.String "vis"
