/**
 * LAM Status Node
 * Retrieves LAM system status including resonance field state
 */

const axios = require('axios');

module.exports = function(RED) {
    function LAMStatusNode(config) {
        RED.nodes.createNode(this, config);
        const node = this;

        node.apiUrl = config.apiUrl || process.env.LAM_API_URL || 'http://localhost:8000';
        node.interval = parseInt(config.interval) || 0;
        node.includeMetrics = config.includeMetrics !== false;

        let intervalId = null;

        async function fetchStatus() {
            try {
                node.status({ fill: "blue", shape: "dot", text: "fetching..." });

                const response = await axios.get(`${node.apiUrl}/status`);

                const msg = {
                    payload: {
                        status: response.data.status,
                        resonance_field: response.data.resonance_field,
                        action_count: response.data.action_count,
                        timestamp: new Date().toISOString()
                    }
                };

                // Optionally fetch metrics as well
                if (node.includeMetrics) {
                    try {
                        const metricsResponse = await axios.get(`${node.apiUrl}/metrics`);
                        msg.payload.metrics = metricsResponse.data;
                    } catch (err) {
                        // Metrics endpoint might not exist, ignore
                        node.warn('Could not fetch metrics: ' + err.message);
                    }
                }

                // Update node status with key info
                const rf = response.data.resonance_field;
                const statusText = rf.stable ?
                    `stable α=${rf.alpha.toFixed(3)}` :
                    `unstable α=${rf.alpha.toFixed(3)}`;

                node.status({
                    fill: rf.stable ? "green" : "yellow",
                    shape: "dot",
                    text: statusText
                });

                node.send(msg);

            } catch (error) {
                node.status({ fill: "red", shape: "ring", text: "error" });
                node.error('Failed to fetch LAM status: ' + error.message);
            }
        }

        node.on('input', function(msg) {
            fetchStatus();
        });

        // Set up polling if interval is configured
        if (node.interval > 0) {
            intervalId = setInterval(fetchStatus, node.interval);
            fetchStatus(); // Fetch immediately on startup
        }

        node.on('close', function() {
            if (intervalId) {
                clearInterval(intervalId);
            }
            node.status({});
        });
    }

    RED.nodes.registerType("lam-status", LAMStatusNode);
};
