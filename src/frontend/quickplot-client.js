"use strict";

/**************************************************************************************/
/* Connectivity                                                                    */
/**************************************************************************************/

var websocketURL = "ws://127.0.0.1:8000/ws";
var websocket;

connectToWebsocket(websocketURL);

/**
 * Connect to the QuickPlot websocket server
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


var plots = [];
var plotsDiv = document.getElementById("plots");


/**
 * Internal plot representation
 * TODO: Make this library agnostic
 *
 * @param json  data to plot
 */
var Plot = function(json) {

    var data = json.data;
    // var layout = json.layout;

    var div = document.createElement("div");
    div.id = "plot" + plots.length;
    div.className = "plot";

    plots.push(this);

    /**
     * Show the plot in the DOM
     */
    this.show = function() {
        if (plotsDiv.firstChild !== undefined) {
            plotsDiv.insertBefore(div, plotsDiv.firstChild);
        } else {
            plotsDiv.appendChild(div);
        }
        Plotly.newPlot(div.id, data);
    };

    /**
     * Remove the plot from the DOM
     */
    this.remove = function() {
        plotsDiv.removeChild(div);
    };
};



/**
 * Map with all the local procedures the server has access to
 */
var procedures = {

    // For project speficic procedures
    QuickPlot : {

        /**
         * Remove all plots from the DOM
         */
        clear : function() {
            for (var i = 0; i < plots.length; i++) {
                plots[i].remove();
            }
            plots = [];
            plotsDiv.className = "show";
        }
    },

    plotly : {

        /**
         * Create a new plot and show it in the browser
         *
         * @param data  data to plot
         */
        newPlot : function(data) {
            var plot = new Plot(data);
            plot.show();
        }

    }
};



/**************************************************************************************/
/* General                                                                         */
/**************************************************************************************/


/**
 * Log to the user and show that message is from QuickPlot
 */
function log(message) {
    console.log("QuickPlot: " + message);
}
