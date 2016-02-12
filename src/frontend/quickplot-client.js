/*
    Known Bugs:
        - vis plots don't work except Network plots
        - vis plot becomes unresponsive once you click it
*/

"use strict";

/**************************************************************************************/
/* Connectivity                                                                    */
/**************************************************************************************/

var websocketURL = "ws://127.0.0.1:8000/ws";
var websocket;

// connectToWebsocket(websocketURL);

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



/* Global options ********************************************************************/


var LIBRARIES = {
    vis : "vis",
    plotly : "plotly"
}

var DEFAULT_PLOT_SETTINGS = {
    sizeX : 8,
    sizeY : 6,
    controlBoxHeight : 50
};

var gridStackOptions = {
    cell_height: 80,
    vertical_margin: 10,
    always_show_resize_handle : true,
    animate : true,
    resizable : {
        handles : 'e, se, s, w, nw, n, ne'
    }
};



/* Plot Manager **********************************************************************/

/*
    All the plots are inside #plots which is a gridstack. Which contains grid-stack-items
    that are controlled by gridstack.js.

    Here is a little visualization

    -------------------------------------------------------------------------
    | grid-stack (aka #plots div)                                           |
    |   |---------------------------------------------------------|         |
    |   | .grid-stack-item                                        |         |
    |   |   |--------------------------------------------------|  |         |
    |   |   |  .grid-stack-item-content                        |  |         |
    |   |   |  |--------------------------------------------|  |  |         |
    |   |   |  | .plot-box                                  |  |  |         |
    |   |   |  |                                            |  |  |         |
    |   |   |  |                                            |  |  |         |
    |   |   |  |                                            |  |  |         |
    |   |   |  |                                            |  |  |         |
    |   |   |  |--------------------------------------------|  |  |         |
    |   |   |  | Here could be controls                     |  |  |         |
    |   |   |  |--------------------------------------------|  |  |         |
    |   |   |--------------------------------------------------|  |         |
    |   |                                                         |         |
    |   |---------------------------------------------------------|         |
    |                                                                       |
    |   |---------------------------------------------------------|         |
    |   | another grid-stack-item                                 |         |
    |   |                                                         |         |
    |   |            ....... the same as above                    |         |
    |   |                                                         |         |
    |   |---------------------------------------------------------|         |
    |                                                                       |
    |-----------------------------------------------------------------------|

*/

// init gridstack
$("#plots").gridstack(gridStackOptions);

/**
 * Handle resizing of the plots by putting an eventlistener on the grid-stack-item
 */

$("#plots").on("resizestop", function(event, ui) {
    var gridStackItem = ui.element[0];
    var plotBox = gridStackItem.childNodes[0].childNodes[0];
    var newSize = {
        width : plotBox.offsetWidth,
        height : (plotBox.offsetHeight - DEFAULT_PLOT_SETTINGS.controlBoxHeight)
    };

    console.log("Default box height", DEFAULT_PLOT_SETTINGS.controlBoxHeight);
    console.log("plotBoxOffset", newSize.height);

    // e.g. plotly_name_id
    switch (gridStackItem.id.split("_")[0]) {
        case LIBRARIES.plotly:
            Plotly.relayout(plotBox, newSize);
            break;
    }
});

var PlotManager = {
    /**
     * Add a new plot to the grid
     * @param data     json data
     * @param library  which library this plot uses
     * @param name     unique name (if not supplied it will be assigned)
     */
    addPlot : function(data, library, name) {

        // Give unique name
        if (name === undefined) {
            name = library + "_plot_" + (new Date()).getTime();
        } else {
            name = library + "_" + name;
        }

        var plot = new Plot(data, name);
        $("#plots").data("gridstack").add_widget(plot.gridStackItem, 0, 0,
                    DEFAULT_PLOT_SETTINGS.sizeX, DEFAULT_PLOT_SETTINGS.sizeY, true);

        $("#no-plots-message").hide();
        debug("Added plot: \"" + plot.name + "\"");

        return plot;
    },

    /**
     * Removes a plot by id (which is also the name)
     */
    removePlotById : function(id) {
        var gridStackItem = $("#" + id).get(0);
        $("#plots").data("gridstack").remove_widget(gridStackItem);
        // TODO: Check if grid is empty then show no-plots-message
        debug("Removed plot: \"" + id + "\"");
    },

    /**
     * Removes all the plots
     */
    removeAllPlots : function() {
        $("#plots").data("gridstack").remove_all();
        $("#no-plots-message").show();
        debug("Removing all plots");
    }
};

/**
 * A plot.
 *
 * Every plot is inside a gridStackItem (to be specific inside gridStackItemContent)
 * and has
 *      - unique name that looks like this "library_name"
 *
 * After you created the plot you still have to put it into the grid
 */

 function Plot(data, name) {

    this.name                 = name;
    this.data                 = data;
    this.gridStackItem        = $("<div id='" + name + "'>");
    this.gridStackItemContent = $("<div class='grid-stack-item-content'>");
    this.plotBox              = $("<div class='plot-box'>");
    this.controlBox           = $("<div class='control-box'>");
    var removeButton          = $("<button class='remove-plot-button'>");

    this.controlBox.append(removeButton);
    this.gridStackItemContent.append(this.plotBox);
    this.gridStackItemContent.append(this.controlBox);
    this.gridStackItem.append(this.gridStackItemContent);

    removeButton.on("click", function() {
        PlotManager.removePlotById(name);
    });

    this.getPlotBox = function() {
        return this.plotBox.get(0);
    };

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
            var plot = PlotManager.addPlot(json.data, LIBRARIES.plotly);
            Plotly.newPlot(plot.getPlotBox(), json.data, json.layout);
        }

    },
    vis : {
        /**
         * Create a new plot and show it in the browser
         * TODO: Only works for networks. Other plot types "Say that drawing is too limiting"
         * @param json  data to plot
         */

        newPlot : function(json) {
            var plot = PlotManager.addPlot(json.data, LIBRARIES.vis);
            var dataset = new vis.DataSet(json.data);
            switch (json.plotType) {
                case "network":
                    var network = new vis.Network(plot.getPlotBox(), json.data, json.options);
                    break;
                case "timeline":
                    var timeline = new vis.Timeline(plot.getPlotBox(), dataset, json.options);
                    break;
                case "graph2d":
                    var graph2d = new vis.Graph2d(plot.getPlotBox(), dataset, json.options);
                    break;
                case "graph3d":
                    var graph3d = new vis.Graph3d(plot.getPlotBox(), dataset, json.options);
                    break;
                default:
                    debug("vis plottype doesn't exist");
            }
            debug("vis newPlot:", json);
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
