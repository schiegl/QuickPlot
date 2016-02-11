/*
    Known Bugs:
        - Vis plots can't be dragged because plot covers all space and occupies the drag event
        - Plotly plots don't resize
        - Resize handle is missing

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
        log("What did you do?? D: ... Trying to reconnect");
        setTimeout(function() { connectToWebsocket(websocketURL); }, 3000);
    };
}


/**************************************************************************************/
/* Plotting                                                                        */
/**************************************************************************************/

var DEFAULT_PLOT_SETTINGS = {
    sizeX : 8,
    sizeY : 6
};

var options = {
    cell_height: 80,
    vertical_margin: 10,
    always_show_resize_handle : true,
    animate : true
};

$("#plots").gridstack(options);


var PlotManager = {

    // The gridstack (aka "plots" div) where all the plots live as "grid-stack-items"
    GRID : $("#plots").data("gridstack"),

    /**
     * Add a new plot to the grid
     */
    addPlot : function(data, name) {

        // Give unique name
        if (name === undefined) {
            name = "plot" + (new Date()).getTime();
        }

        var plot = new Plot(data, name);
        this.GRID.add_widget(plot.gridStackItem, 0, 0,
                    DEFAULT_PLOT_SETTINGS.sizeX, DEFAULT_PLOT_SETTINGS.sizeY, true);

        $("#noPlotsMessage").hide();
        debug("Added plot: \"" + plot.name + "\"");

        return plot;
    },

    /**
     * Removes a specific plot
     */
    removePlot : function(plot) {
        this.GRID.remove(plot);
        // TODO: Check if grid is empty then show noPlotsMessage
        debug("Removed plot: \"" + plot.name + "\"");
    },

    /**
     * Removes all the plots
     */
    removeAllPlots : function() {
        this.GRID.remove_all();
        $("#noPlotsMessage").show();
        debug("Removing all plots");
    }
}

/**
 * A plot with a unique name
 *
 * Data might not be shown yet
 */

 function Plot(data, name) {

    this.name                 = name;
    this.data                 = data;
    this.gridStackItem        = $("<div id='" + name + "'>");
    this.gridStackItemContent = $("<div class='grid-stack-item-content'>");
    this.plotArea             = $("<div class='plotArea'>");
    this.gridStackItemContent.append(this.plotArea);
    this.gridStackItem.append(this.gridStackItemContent);

    this.getPlotArea = function() {
        return this.plotArea.get(0);
    }

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
            debug("clear");
            PlotManager.removeAllPlots();
        }
    },

    plotly : {

        /**
         * Create a new plot and show it in the browser
         *
         * @param json  data to plot
         */
        newPlot : function(json) {
            debug("plotly newPlot:", json);
            var plot = PlotManager.addPlot(json.data);
            Plotly.newPlot(plot.getPlotArea(), json.data, json.layout);
        }

    },
    vis : {
        /**
         * Create a new plot and show it in the browser
         *
         * @param json  data to plot
         */
        newPlot : function(json) {
            debug("vis newPlot:", json);
            var plot = PlotManager.addPlot(json.data);
            var network = new vis.Network(plot.getPlotArea(), json.data, json.options);
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
function debug(one, two, three, four) {
    if (show_debug_messages) {
        if (two === undefined) {
            console.log("DEBUG || ", one);
        } else if (three === undefined) {
            console.log("DEBUG || ", one, two);
        } else if (four === undefined){
            console.log("DEBUG || ", one, two, three);
        } else {
            console.log("DEBUG || ", one, two, three, four);
        }
    }
}


/**
 * Log to the user and show that message is from QuickPlot
 */
function log(message) {
    console.log("QuickPlot: " + message);
}
