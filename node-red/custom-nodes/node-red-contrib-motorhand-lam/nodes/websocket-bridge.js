/**
 * WebSocket Bridge Node
 * Two-way bridge to LAM WebSocket server (port 8765)
 */

const WebSocket = require('ws');

module.exports = function(RED) {
    function WebSocketBridgeNode(config) {
        RED.nodes.createNode(this, config);
        const node = this;

        node.wsUrl = config.wsUrl || 'ws://host.docker.internal:8765';
        node.autoReconnect = config.autoReconnect !== false;
        node.reconnectInterval = parseInt(config.reconnectInterval) || 5000;

        let ws = null;
        let reconnectTimer = null;
        let isClosing = false;

        function connect() {
            if (isClosing) return;

            try {
                node.status({ fill: "yellow", shape: "ring", text: "connecting..." });

                ws = new WebSocket(node.wsUrl);

                ws.on('open', function() {
                    node.status({ fill: "green", shape: "dot", text: "connected" });
                    node.log('WebSocket connected to ' + node.wsUrl);
                });

                ws.on('message', function(data) {
                    try {
                        const parsed = JSON.parse(data);
                        node.send({
                            payload: parsed,
                            topic: parsed.type || 'message',
                            _websocket: 'incoming'
                        });
                    } catch (err) {
                        // Not JSON, send as-is
                        node.send({
                            payload: data.toString(),
                            _websocket: 'incoming'
                        });
                    }
                });

                ws.on('error', function(err) {
                    node.status({ fill: "red", shape: "ring", text: "error" });
                    node.error('WebSocket error: ' + err.message);
                });

                ws.on('close', function() {
                    node.status({ fill: "red", shape: "ring", text: "disconnected" });

                    if (!isClosing && node.autoReconnect) {
                        node.log('WebSocket closed, reconnecting in ' + node.reconnectInterval + 'ms');
                        reconnectTimer = setTimeout(connect, node.reconnectInterval);
                    }
                });

            } catch (err) {
                node.status({ fill: "red", shape: "ring", text: "error" });
                node.error('Failed to connect: ' + err.message);

                if (node.autoReconnect && !isClosing) {
                    reconnectTimer = setTimeout(connect, node.reconnectInterval);
                }
            }
        }

        // Send messages to WebSocket
        node.on('input', function(msg) {
            if (!ws || ws.readyState !== WebSocket.OPEN) {
                node.warn('WebSocket not connected, message not sent');
                return;
            }

            try {
                let data;

                // If payload is object, stringify it
                if (typeof msg.payload === 'object') {
                    data = JSON.stringify(msg.payload);
                } else {
                    data = msg.payload.toString();
                }

                ws.send(data);

                node.status({
                    fill: "green",
                    shape: "dot",
                    text: "sent: " + new Date().toLocaleTimeString()
                });

            } catch (err) {
                node.error('Failed to send message: ' + err.message);
            }
        });

        // Connect on startup
        connect();

        node.on('close', function() {
            isClosing = true;

            if (reconnectTimer) {
                clearTimeout(reconnectTimer);
            }

            if (ws) {
                ws.close();
            }

            node.status({});
        });
    }

    RED.nodes.registerType("websocket-bridge", WebSocketBridgeNode);
};
