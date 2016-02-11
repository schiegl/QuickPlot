QuickPlot
=========

Quick and easy data visualizations with Haskell

![QuickPlot Demo](https://raw.githubusercontent.com/tepf/QuickPlot/master/QuickPlotDemo.gif)

How does it work?
----------------

Haskell creates a simple server that runs in the background and sends data to a browser that visualizes it. In the demo above the browser and ghci runs inside [Atom](https://atom.io), but you can use any editor (even [butterflies](https://xkcd.com/378/)) with a fairly modern browser. Once the data arrives at the browser any JavaScript visualization library could take care of it like for example [plot.ly](https://plot.ly/javascript/)


How do I use it?
----------------

This library is not ready to use just yet. The visualization libraries are not fully implemented.


FAQ
---

#### Does it work on X?

Probably yes. If Linux can run on a potato so can this. But jokes aside. [If you updated your browser in the last couple of years](http://caniuse.com/websockets) and you are allowed to have a local server running at your localhost address then it will work.

#### Can you implement library X?

If the library takes JSON as an input, submit an issue. Libraries like [D3.js](https://d3js.org) are problematic however. Graphs in D3 are created programatically and therefore you can't just pick a graph type and say plot it. You have to create them from scratch. If there are examples that give you goosebumps it is possible to integrate them separately. Or you just include it as a user script yourself.

#### How can I help?

There are many ways to do so:

- If you know JavaScript/HTML/CSS you could make the UI prettier, more user friendly or easier extensible
- Implement a visualization library in JavaScript

#### Can this library only use JavaScript libraries?

Technically any library that Haskell can convice to create graphics is compatible.
