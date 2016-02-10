{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module QuickPlot.Plotly (
) where

import QuickPlot
import QuickPlot.IPC.Protocol


instance Plottable PlotlyJSON where
    toJSON (PlotlyJSON trace) = [json|{
                                    data : [ #{ trace } ]
                                }|]
    getLibrary _ = Plotly

instance Plottable [PlotlyJSON] where
    toJSON traces = [json|{
                        data : #{ traces }
                    }|]
    getLibrary _ = Plotly

instance Plottable (PlotlyJSON, PlotlyJSON) where
    toJSON (traces, layout) = [json|{
                                  data : [ #{ traces } ]
                                , layout : #{ layout }
                              }|]
    getLibrary _ = Plotly

instance Plottable ([PlotlyJSON], PlotlyJSON) where
    toJSON (traces, layout) = [json|{
                                  data : #{ traces }
                                , layout : #{ layout }
                              }|]
    getLibrary _ = Plotly
