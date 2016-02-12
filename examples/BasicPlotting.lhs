> {-# LANGUAGE QuasiQuotes #-} -- Don't forget them QuasiQuotes

> module Main where

> import QuickPlot
> import QuickPlot.Plotly
> import Control.Concurrent


As box aficionados we just measured the weight of all our boxes at home
and gathered the following data (if you don't have boxes ask your neighbor)

> blueBoxSizes   = [1.1, 1.8, 2.9, 3.3] :: [Double]
> orangeBoxSizes = [3.1, 3.8, 5.9, 2.3] :: [Double]


Now we're interested in visualizing our data and decide to go with theme
and use BoxPlots. Specifically, we will use plot.ly's data visualization
capabilities. You can find more about them on their website.

In plot.ly every dataset you want to visualize is called a trace.
Such a trace is created in JSON (JavaScript Object Notation).

Here is an example for JSON:

        {
            "y"    : [1.1, 1.8, 0.9, 1.3],
            "type" : "box"
        }

This is how we would write it in our program:

> blueBoxTrace = [plotly|{
>                           y    : #{ blueBoxSizes },
>                           type : "box"
>                        }|]

The element before the ":" is called a key and the element after a value.

But because we're in Haskell we can't just write JSON directly.
We have to put them inside quasiquotes

    e.g. [quasiquote| ... JSON ... JSON ... |]

Instead of "quasiquote" we write the library this JSON was meant for.

Even then we can't write 100% true JSON. We have to write them similar
to JavaScript objects. This means, we lose the """ (double-quotes)
for the keys and when we want to embed Haskell code into the JSON we
need to put it inside "#{}".

Knowing all that here is the second trace for the orange boxes:

> orangeBoxTrace = [plotly|{
>                            y : #{ orangeBoxSizes },
>                            type : "box"
>                          }|]


It's time to plot now and gain great insight from our data.
Plotting works different depending on the library you want to use
but always with the plot function.

Since plot does IO we will put it into main. But you would want to
use the plot function interactively in ghci.

> main :: IO ()
> main = do
>    runQuickPlot
>    putStrLn "Quick! You have 15 seconds to open your browser and visit the address above me"
>    threadDelay 3000000 -- wait 15 seconds
>    plot [blueBoxTrace, orangeBoxTrace]
