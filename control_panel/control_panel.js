/**
 * MotorHandPro Control Panel JavaScript
 * Handles WebSocket communication, visualization, and system control
 *
 * Copyright 2025 Donte Lightfoot - The Phoney Express LLC / Locked In Safety
 * Patent Pending: U.S. Provisional Patent Application No. 63/842,846
 */

class ControlPanel {
    constructor() {
        this.ws = null;
        this.charts = {};
        this.dataBuffer = [];
        this.scene = null;
        this.camera = null;
        this.renderer = null;
        this.animationId = null;

        // Primal Logic parameters
        this.params = {
            lambda: 0.16905,
            ke: 0.3,
            d_constant: 149.9992314000
        };

        this.init();
    }

    init() {
        this.setupWebSocket();
        this.setupEventListeners();
        this.setupCharts();
        this.setup3DVisualization();
        this.setupTabs();
    }

    // ==================== WEBSOCKET CONNECTION ====================

    setupWebSocket() {
        const wsUrl = 'ws://localhost:8765';

        this.ws = new WebSocket(wsUrl);

        this.ws.onopen = () => {
            console.log('Connected to MotorHandPro server');
            this.updateConnectionStatus(true);
        };

        this.ws.onmessage = (event) => {
            const data = JSON.parse(event.data);
            this.handleIncomingData(data);
        };

        this.ws.onerror = (error) => {
            console.error('WebSocket error:', error);
            this.updateConnectionStatus(false);
        };

        this.ws.onclose = () => {
            console.log('Disconnected from server');
            this.updateConnectionStatus(false);

            // Attempt reconnection after 5 seconds
            setTimeout(() => {
                console.log('Attempting to reconnect...');
                this.setupWebSocket();
            }, 5000);
        };
    }

    updateConnectionStatus(connected) {
        const statusEl = document.getElementById('connection-status');
        if (connected) {
            statusEl.textContent = 'Connected';
            statusEl.classList.remove('disconnected');
            statusEl.classList.add('connected');
        } else {
            statusEl.textContent = 'Disconnected';
            statusEl.classList.remove('connected');
            statusEl.classList.add('disconnected');
        }
    }

    // ==================== DATA HANDLING ====================

    handleIncomingData(data) {
        console.log('Received data:', data);

        if (data.type === 'visualization_update') {
            this.updateVisualization(data.data);
        } else if (data.type === 'control_update') {
            this.updateControlPanel(data.data);
        }

        this.updateDataStream(data);
        this.dataBuffer.push(data);

        // Keep buffer at reasonable size
        if (this.dataBuffer.length > 1000) {
            this.dataBuffer.shift();
        }
    }

    updateDataStream(data) {
        const streamContainer = document.getElementById('data-stream');

        // Remove "no data" message
        const noDataMsg = streamContainer.querySelector('.no-data');
        if (noDataMsg) {
            noDataMsg.remove();
        }

        // Create new entry
        const entry = document.createElement('div');
        entry.className = 'data-entry';

        const timestamp = new Date(data.timestamp).toLocaleTimeString();
        const source = data.data?.source || 'Unknown';

        entry.innerHTML = `
            <div class="timestamp">${timestamp}</div>
            <div class="source">Source: ${source}</div>
            <div class="value">${JSON.stringify(data.data, null, 2)}</div>
        `;

        streamContainer.insertBefore(entry, streamContainer.firstChild);

        // Keep only last 20 entries
        while (streamContainer.children.length > 20) {
            streamContainer.removeChild(streamContainer.lastChild);
        }
    }

    // ==================== VISUALIZATION UPDATES ====================

    updateVisualization(data) {
        // Update real-time chart
        if (this.charts.realtime && data.primal_logic_analysis) {
            const analysis = data.primal_logic_analysis;

            this.charts.realtime.data.labels.push(new Date().toLocaleTimeString());
            this.charts.realtime.data.datasets[0].data.push(analysis.control_energy || 0);
            this.charts.realtime.data.datasets[1].data.push(analysis.stability_metric || 0);

            // Keep only last 50 points
            if (this.charts.realtime.data.labels.length > 50) {
                this.charts.realtime.data.labels.shift();
                this.charts.realtime.data.datasets.forEach(dataset => dataset.data.shift());
            }

            this.charts.realtime.update('none'); // Update without animation for performance
        }

        // Update stability chart
        if (this.charts.stability && data.primal_logic_analysis) {
            const lipschitz = data.primal_logic_analysis.lipschitz_estimate;
            if (lipschitz !== null && lipschitz !== undefined) {
                this.charts.stability.data.datasets[0].data.push({
                    x: Date.now(),
                    y: lipschitz
                });

                this.charts.stability.update('none');
            }
        }

        // Update 3D visualization
        this.update3DScene(data);
    }

    updateControlPanel(data) {
        // Update system status
        const dataRate = Math.floor(Math.random() * 50) + 10; // Simulated
        const processingLoad = Math.floor(Math.random() * 30) + 20; // Simulated
        const activeSources = Object.keys(data).length;

        document.getElementById('data-rate').textContent = dataRate;
        document.getElementById('processing-load').textContent = processingLoad;
        document.getElementById('active-sources').textContent = activeSources;
    }

    // ==================== CHARTS SETUP ====================

    setupCharts() {
        // Real-time Chart
        const realtimeCtx = document.getElementById('realtime-chart').getContext('2d');
        this.charts.realtime = new Chart(realtimeCtx, {
            type: 'line',
            data: {
                labels: [],
                datasets: [
                    {
                        label: 'Control Energy',
                        data: [],
                        borderColor: '#00d4ff',
                        backgroundColor: 'rgba(0, 212, 255, 0.1)',
                        tension: 0.4
                    },
                    {
                        label: 'Stability Metric',
                        data: [],
                        borderColor: '#00ff88',
                        backgroundColor: 'rgba(0, 255, 136, 0.1)',
                        tension: 0.4
                    }
                ]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                plugins: {
                    title: {
                        display: true,
                        text: 'Real-time Primal Logic Metrics',
                        color: '#fff'
                    },
                    legend: {
                        labels: { color: '#fff' }
                    }
                },
                scales: {
                    x: { ticks: { color: '#fff' }, grid: { color: 'rgba(255,255,255,0.1)' } },
                    y: { ticks: { color: '#fff' }, grid: { color: 'rgba(255,255,255,0.1)' } }
                }
            }
        });

        // Stability Chart
        const stabilityCtx = document.getElementById('stability-chart').getContext('2d');
        this.charts.stability = new Chart(stabilityCtx, {
            type: 'scatter',
            data: {
                datasets: [{
                    label: 'Lipschitz Constant',
                    data: [],
                    borderColor: '#ffaa00',
                    backgroundColor: 'rgba(255, 170, 0, 0.5)',
                    showLine: true
                }]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                plugins: {
                    title: {
                        display: true,
                        text: 'Stability Analysis (Lipschitz < 1 for convergence)',
                        color: '#fff'
                    },
                    legend: {
                        labels: { color: '#fff' }
                    }
                },
                scales: {
                    x: {
                        type: 'linear',
                        ticks: { color: '#fff' },
                        grid: { color: 'rgba(255,255,255,0.1)' }
                    },
                    y: {
                        ticks: { color: '#fff' },
                        grid: { color: 'rgba(255,255,255,0.1)' },
                        max: 2
                    }
                }
            }
        });

        // Trajectory Chart
        const trajectoryCtx = document.getElementById('trajectory-chart').getContext('2d');
        this.charts.trajectory = new Chart(trajectoryCtx, {
            type: 'line',
            data: {
                datasets: [{
                    label: 'Vehicle Trajectory',
                    data: [],
                    borderColor: '#ff6600',
                    backgroundColor: 'rgba(255, 102, 0, 0.1)',
                }]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                plugins: {
                    title: {
                        display: true,
                        text: '3D Trajectory Projection (X-Y Plane)',
                        color: '#fff'
                    }
                },
                scales: {
                    x: {
                        type: 'linear',
                        title: { display: true, text: 'X Position', color: '#fff' },
                        ticks: { color: '#fff' },
                        grid: { color: 'rgba(255,255,255,0.1)' }
                    },
                    y: {
                        title: { display: true, text: 'Y Position', color: '#fff' },
                        ticks: { color: '#fff' },
                        grid: { color: 'rgba(255,255,255,0.1)' }
                    }
                }
            }
        });
    }

    // ==================== 3D VISUALIZATION (three.js) ====================

    setup3DVisualization() {
        const container = document.getElementById('threejs-container');

        // Scene
        this.scene = new THREE.Scene();
        this.scene.background = new THREE.Color(0x0a0a0a);

        // Camera
        this.camera = new THREE.PerspectiveCamera(
            75,
            container.clientWidth / container.clientHeight,
            0.1,
            1000
        );
        this.camera.position.z = 50;

        // Renderer
        this.renderer = new THREE.WebGLRenderer({ antialias: true });
        this.renderer.setSize(container.clientWidth, container.clientHeight);
        container.appendChild(this.renderer.domElement);

        // Lights
        const ambientLight = new THREE.AmbientLight(0x404040);
        this.scene.add(ambientLight);

        const directionalLight = new THREE.DirectionalLight(0xffffff, 0.5);
        directionalLight.position.set(1, 1, 1);
        this.scene.add(directionalLight);

        // Add grid
        const gridHelper = new THREE.GridHelper(100, 20, 0x00d4ff, 0x444444);
        this.scene.add(gridHelper);

        // Add coordinate axes
        const axesHelper = new THREE.AxesHelper(25);
        this.scene.add(axesHelper);

        // Add a sample object (will be updated with real data)
        const geometry = new THREE.BoxGeometry(5, 5, 5);
        const material = new THREE.MeshPhongMaterial({ color: 0x00d4ff });
        this.vehicle = new THREE.Mesh(geometry, material);
        this.scene.add(this.vehicle);

        // Animation loop
        this.animate3D();

        // Handle window resize
        window.addEventListener('resize', () => {
            this.camera.aspect = container.clientWidth / container.clientHeight;
            this.camera.updateProjectionMatrix();
            this.renderer.setSize(container.clientWidth, container.clientHeight);
        });
    }

    animate3D() {
        this.animationId = requestAnimationFrame(() => this.animate3D());

        // Rotate vehicle slowly
        if (this.vehicle) {
            this.vehicle.rotation.x += 0.005;
            this.vehicle.rotation.y += 0.01;
        }

        this.renderer.render(this.scene, this.camera);
    }

    update3DScene(data) {
        // Update 3D visualization based on incoming data
        if (data.payload) {
            const payload = data.payload;

            // Update vehicle position if available
            if (payload.position) {
                const pos = payload.position;
                this.vehicle.position.set(
                    pos.x / 10 || 0,
                    pos.z / 10 || 0,
                    pos.y / 10 || 0
                );
            }

            // Update vehicle rotation if available
            if (payload.attitude) {
                const att = payload.attitude;
                this.vehicle.rotation.set(
                    att.pitch || 0,
                    att.yaw || 0,
                    att.roll || 0
                );
            }
        }
    }

    // ==================== EVENT LISTENERS ====================

    setupEventListeners() {
        // Parameter sliders
        document.getElementById('lambda-slider').addEventListener('input', (e) => {
            this.params.lambda = parseFloat(e.target.value);
            document.getElementById('lambda-value').textContent = e.target.value;
        });

        document.getElementById('ke-slider').addEventListener('input', (e) => {
            this.params.ke = parseFloat(e.target.value);
            document.getElementById('ke-value').textContent = e.target.value;
        });

        // Update parameters button
        document.getElementById('update-parameters').addEventListener('click', () => {
            this.sendControlCommand('parameter_update', this.params);
        });

        // Control buttons
        document.getElementById('start-capture').addEventListener('click', () => {
            this.sendControlCommand('start_capture');
        });

        document.getElementById('stop-capture').addEventListener('click', () => {
            this.sendControlCommand('stop_capture');
        });

        document.getElementById('export-data').addEventListener('click', () => {
            this.exportToLatex();
        });

        document.getElementById('run-validation').addEventListener('click', () => {
            this.runValidation();
        });
    }

    setupTabs() {
        const tabButtons = document.querySelectorAll('.tab-btn');
        const tabContents = document.querySelectorAll('.tab-content');

        tabButtons.forEach(button => {
            button.addEventListener('click', () => {
                const tabName = button.getAttribute('data-tab');

                // Remove active class from all
                tabButtons.forEach(btn => btn.classList.remove('active'));
                tabContents.forEach(content => content.classList.remove('active'));

                // Add active class to clicked
                button.classList.add('active');
                document.getElementById(`tab-${tabName}`).classList.add('active');
            });
        });
    }

    // ==================== CONTROL COMMANDS ====================

    sendControlCommand(type, data = {}) {
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            const message = {
                type: type === 'parameter_update' ? 'parameter_update' : 'control_command',
                command: type,
                data: data,
                timestamp: new Date().toISOString()
            };

            this.ws.send(JSON.stringify(message));
            console.log('Sent command:', message);
        } else {
            alert('Not connected to server');
        }
    }

    exportToLatex() {
        this.sendControlCommand('export_latex', {
            repository: 'All',
            lambda: this.params.lambda,
            lipschitz: 0.000129931830,
            control_energy: this.dataBuffer.length > 0 ?
                this.dataBuffer[this.dataBuffer.length - 1].data?.primal_logic_analysis?.control_energy : 0
        });

        alert('LaTeX export requested. Check server logs for output location.');
    }

    runValidation() {
        this.sendControlCommand('run_validation', {
            repositories: ['SpaceX-API', 'Tesla-light-show', 'PX4-Autopilot', 'CARLA-Simulator'],
            parameters: this.params
        });

        alert('Validation started. Results will appear in the data stream.');
    }
}

// Initialize control panel when page loads
document.addEventListener('DOMContentLoaded', () => {
    const controlPanel = new ControlPanel();
    console.log('MotorHandPro Control Panel initialized');
});
