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


Can I help?
-----------

Yup! The biggest points on my TODO list are:

 - think of a versatile way to interface as many JSON APIs and JavaScript libraries as possible
 - write an UI in HTML/CSS/JS that manages the visualizations
 - make the server more resilient towards unexpected events (reloads, interrupts)
