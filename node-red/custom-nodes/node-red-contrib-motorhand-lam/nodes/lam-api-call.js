/**
 * LAM API Call Node
 * Makes HTTP requests to MotorHandPro LAM API endpoints
 */

const axios = require('axios');

module.exports = function(RED) {
    function LAMAPICallNode(config) {
        RED.nodes.createNode(this, config);
        const node = this;

        // Configuration
        node.apiUrl = config.apiUrl || process.env.LAM_API_URL || 'http://localhost:8000';
        node.endpoint = config.endpoint;
        node.method = config.method || 'POST';
        node.timeout = parseInt(config.timeout) || 30000;

        node.on('input', async function(msg, send, done) {
            // For Node-RED 0.x compatibility
            send = send || function() { node.send.apply(node, arguments); };
            done = done || function(err) { if (err) node.error(err, msg); };

            try {
                // Set status to processing
                node.status({ fill: "blue", shape: "dot", text: "calling API..." });

                // Determine endpoint from config or msg
                const endpoint = msg.endpoint || node.endpoint;
                if (!endpoint) {
                    throw new Error('No endpoint specified');
                }

                // Build full URL
                const url = `${node.apiUrl}${endpoint.startsWith('/') ? '' : '/'}${endpoint}`;

                // Prepare request configuration
                const requestConfig = {
                    method: node.method,
                    url: url,
                    timeout: node.timeout,
                    headers: {
                        'Content-Type': 'application/json',
                        ...msg.headers
                    }
                };

                // Add payload for POST/PUT requests
                if (node.method === 'POST' || node.method === 'PUT') {
                    requestConfig.data = msg.payload || {};
                }

                // Add query parameters for GET requests
                if (node.method === 'GET' && msg.query) {
                    requestConfig.params = msg.query;
                }

                // Make API call
                const startTime = Date.now();
                const response = await axios(requestConfig);
                const duration = Date.now() - startTime;

                // Prepare output message
                msg.payload = response.data;
                msg.statusCode = response.status;
                msg.headers = response.headers;
                msg.responseTime = duration;

                // Update status
                node.status({ fill: "green", shape: "dot", text: `success (${duration}ms)` });

                // Send message
                send(msg);
                done();

            } catch (error) {
                // Handle errors
                node.status({ fill: "red", shape: "ring", text: "error" });

                msg.error = {
                    message: error.message,
                    code: error.code,
                    statusCode: error.response?.status,
                    data: error.response?.data
                };

                // Send error message to second output
                send([null, msg]);
                done(error);
            }
        });

        node.on('close', function() {
            node.status({});
        });
    }

    RED.nodes.registerType("lam-api-call", LAMAPICallNode);
};
