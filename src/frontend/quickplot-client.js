/*
    Known Bugs:
        - Plot containers are not in sync with plot
        - Plot doesn't resize on window resize
*/

"use strict";

/**************************************************************************************/
/* Connectivity                                                                    */
/**************************************************************************************/

var websocketURL = "ws://127.0.0.1:8000/ws";
var websocket;

connectToWebsocket(websocketURL);

/**
 * Connect to the QuickPlot websocket server
 * @param websocketURL  url of the websocket
 */

function connectToWebsocket(websocketURL) {

    websocket = new WebSocket(websocketURL);

    websocket.onmessage = function(event) {
        var message = JSON.parse(event.data);
        procedures[message.library][message.procedure](message.content);
    };

    websocket.onopen = function(event) {
        log("Connected to QuickPlot");
    };

    websocket.onclose = function(event) {
        log("What did you do? Trying to reconnect ... ");
        setTimeout(function() { connectToWebsocket(websocketURL); }, 3000);
    };
}


/**************************************************************************************/
/* Plotting                                                                        */
/**************************************************************************************/

var plotManager = new PlotManager();


function PlotManager() {

    var numOfPlots = 0;
    this.container = document.getElementById("plots");

    this.addPlot = function(data, name) {

        if (name === undefined) {
            name = "plot" + numOfPlots++;
        }

        var plot = new Plot(data, name);
        plot.removeButton.addEventListener("click", function() {
             plotManager.removePlot(plot);
        });

        this.container.insertBefore(plot.DOMNode, this.container.firstChild);

        hide("noPlotsMessage"); // can hide message now because user knows how to create a plot
        debug("Added plot: \"" + plot.name + "\"");

        return plot;
    }

    this.removePlot = function(plot) {
        this.container.removeChild(plot.DOMNode);
        debug("Removed plot: \"" + plot.name + "}\"");
    }

    this.removeAllPlots = function() {
        var current = this.container;
        while (current.firstChild) {
            current.removeChild(current.firstChild);
        }
        debug("Removing all plots");
    }
}


 function Plot(data, name) {

    this.name         = name
    this.data         = data;
    this.DOMNode      = document.createElement("div");
    this.plotArea     = document.createElement("div");
    this.controlArea  = document.createElement("div")
    this.removeButton = document.createElement("a");

    this.plotArea.setAttribute("class", "plotArea");
    this.plotArea.setAttribute("class", "controlArea");
    this.DOMNode.setAttribute("class", "plot");
    this.DOMNode.setAttribute("id", name);
    this.removeButton.setAttribute("class", "removePlotButton");

    this.DOMNode.appendChild(this.plotArea);
    this.DOMNode.appendChild(this.controlArea);
    this.controlArea.appendChild(this.removeButton);

    debug("Created plot with name: " + name);

 }



/**
 * Map with all the local methods the server can execute
 */
var procedures = {

    // For project speficic procedures
    QuickPlot : {

        /**
         * Remove all plots from the DOM
         */
        clear : function() {
            plotManager.removeAllPlots();
        }
    },

    plotly : {

        /**
         * Create a new plot and show it in the browser
         *
         * @param json  data to plot
         */
        newPlot : function(json) {
            var plot = plotManager.addPlot(json.data);
            Plotly.newPlot(plot.plotArea, json.data, json.layout);
        }

    }
};



/**************************************************************************************/
/* General                                                                         */
/**************************************************************************************/

var show_debug_messages = true;

/**
 * If show_debug_messages is true then message will be printed to console
 * @param message    message to print
 */
function debug(message) {
    if (show_debug_messages) {
        console.log("DEBUG: " + message);
    }
}


/**
 * Log to the user and show that message is from QuickPlot
 */
function log(message) {
    console.log("QuickPlot: " + message);
}

/**
 * Hide an element
 * @param element   id of the element
 */
function hide(element) {
    document.getElementById(element).style.display = "none";
    debug("Hiding element: \"" + element + "\"");
}

/**
 * Show an element
 * @param element   id of the element
 */
function show(element) {
    document.getElementById(element).style.display = "block";
    debug("Showing element: \"" + element + "\"");
}
