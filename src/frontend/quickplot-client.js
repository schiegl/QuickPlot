"use strict";

var websocketURL = "ws://127.0.0.1:8000/ws";
var plots = [];
var plotsDiv = document.getElementById("plots");

function log(message) {
    console.log("QuickPlot: " + message);
}

var Plot = function(json) {

    var data = json.data;
    // var layout = json.layout;

    var div = document.createElement("div");
    div.id = "plot" + plots.length;
    div.className = "plot";

    plots.push(this);

    this.show = () => {
        if (plotsDiv.firstChild !== undefined) {
            plotsDiv.insertBefore(div, plotsDiv.firstChild);
        } else {
            plotsDiv.appendChild(div);
        }
        Plotly.newPlot(div.id, data);
    };

    this.remove = () => {
        plotsDiv.removeChild(div);
    };
};

var procedures = {
    general : {
        clear : function() {
            for (var i = 0; i < plots.length; i++) {
                plots[i].remove();
            }
            plots = [];
            plotsDiv.className = "show";
        }
    },
    plotly : {
        newPlot : function(data) {
            var plot = new Plot(data);
            plot.show();
        }
    }
};

var websocket;

function connectToWebsocket(websocketURL) {

    log("Connecting to Haskell program...");

    websocket = new WebSocket(websocketURL);

    websocket.onmessage = function(event) {
        var message = JSON.parse(event.data);
        procedures[message.library][message.procedure](message.content);
    };

    // websocket.onopen = function(event) {
    // };

    websocket.onclose = function(event) {
        log("Lost connection to Haskell program");
        setTimeout(() => connectToWebsocket(websocketURL), 3000);
    };
}

connectToWebsocket(websocketURL);
