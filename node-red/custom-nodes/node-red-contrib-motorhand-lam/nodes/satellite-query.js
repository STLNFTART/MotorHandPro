/**
 * Satellite Query Node
 * Query satellite constellation system
 */

const axios = require('axios');

module.exports = function(RED) {
    function SatelliteQueryNode(config) {
        RED.nodes.createNode(this, config);
        const node = this;

        node.apiUrl = config.apiUrl || process.env.LAM_API_URL || 'http://localhost:8000';
        node.queryType = config.queryType || 'status';
        node.satelliteId = config.satelliteId;

        node.on('input', async function(msg, send, done) {
            send = send || function() { node.send.apply(node, arguments); };
            done = done || function(err) { if (err) node.error(err, msg); };

            try {
                node.status({ fill: "blue", shape: "dot", text: "querying..." });

                const queryType = msg.queryType || node.queryType;
                let endpoint = '';
                let params = {};

                switch (queryType) {
                    case 'status':
                        // Get satellite system status
                        endpoint = '/api/status';
                        break;

                    case 'satellite':
                        // Get specific satellite by ID
                        const satId = msg.satelliteId || node.satelliteId;
                        if (!satId) {
                            throw new Error('Satellite ID required for satellite query');
                        }
                        endpoint = `/api/satellite/${satId}`;
                        break;

                    case 'batch':
                        // Query multiple satellites
                        if (!msg.payload || !msg.payload.satellite_ids) {
                            throw new Error('satellite_ids array required for batch query');
                        }
                        endpoint = '/api/satellites/batch';
                        // This will be a POST request
                        break;

                    case 'visible':
                        // Get visible satellites from location
                        const lat = msg.latitude || msg.payload?.latitude;
                        const lon = msg.longitude || msg.payload?.longitude;

                        if (!lat || !lon) {
                            throw new Error('Latitude and longitude required for visible query');
                        }

                        endpoint = '/api/visible';
                        params = { latitude: lat, longitude: lon };

                        // Optional: add altitude
                        if (msg.altitude || msg.payload?.altitude) {
                            params.altitude = msg.altitude || msg.payload.altitude;
                        }
                        break;

                    case 'coverage':
                        // Get global coverage analysis
                        endpoint = '/api/coverage';

                        // Optional location for regional coverage
                        if (msg.latitude && msg.longitude) {
                            params = {
                                latitude: msg.latitude,
                                longitude: msg.longitude,
                                radius: msg.radius || 1000 // km
                            };
                        }
                        break;

                    case 'constellation':
                        // Get constellation metadata
                        endpoint = '/api/constellation';
                        break;

                    default:
                        throw new Error(`Unknown query type: ${queryType}`);
                }

                // Make API request
                const startTime = Date.now();
                let response;

                if (queryType === 'batch') {
                    response = await axios.post(`${node.apiUrl}${endpoint}`, msg.payload);
                } else {
                    response = await axios.get(`${node.apiUrl}${endpoint}`, { params });
                }

                const duration = Date.now() - startTime;

                // Prepare output
                msg.payload = response.data;
                msg.queryType = queryType;
                msg.responseTime = duration;
                msg.satelliteCount = Array.isArray(response.data) ?
                    response.data.length :
                    (response.data.satellite_count || 1);

                // Update status
                node.status({
                    fill: "green",
                    shape: "dot",
                    text: `${msg.satelliteCount} sats (${duration}ms)`
                });

                send(msg);
                done();

            } catch (error) {
                node.status({ fill: "red", shape: "ring", text: "error" });
                node.error('Satellite query failed: ' + error.message, msg);
                done(error);
            }
        });

        node.on('close', function() {
            node.status({});
        });
    }

    RED.nodes.registerType("satellite-query", SatelliteQueryNode);
};
