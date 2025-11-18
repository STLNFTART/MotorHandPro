/**
 * Node-RED Settings for MotorHandPro Integration
 *
 * This file configures Node-RED for production deployment with security,
 * context storage, and custom function node capabilities.
 */

module.exports = {
    /*******************************************************************************
     * Runtime Settings
     ******************************************************************************/

    // Port used by Node-RED
    uiPort: process.env.PORT || 1880,

    // Retry time in milliseconds for MQTT connections
    mqttReconnectTime: 15000,

    // Retry time in milliseconds for Serial port connections
    serialReconnectTime: 15000,

    // Timeout in milliseconds for TCP server socket connections
    socketReconnectTime: 10000,

    // Maximum message size for TCP server socket connections
    socketTimeout: 120000,

    // Maximum message size for MQTT and TCP
    debugMaxLength: 1000,

    /*******************************************************************************
     * Security
     ******************************************************************************/

    // Uncomment the following to enable secure comms on TLS
    // requireHttps: true,

    // Admin authentication
    adminAuth: {
        type: "credentials",
        users: [{
            username: "admin",
            password: process.env.NODE_RED_ADMIN_PASSWORD_HASH || "$2b$08$6QSxE6c/rNbZLrjrQGHvV.ZLXLhkl0RMtFQnJVhLaQHZZYlNEFpWC", // default: admin
            permissions: "*"
        }]
    },

    // Editor authentication (same as admin for now)
    httpNodeAuth: {
        user: "user",
        pass: process.env.NODE_RED_HTTP_PASSWORD_HASH || "$2b$08$6QSxE6c/rNbZLrjrQGHvV.ZLXLhkl0RMtFQnJVhLaQHZZYlNEFpWC"
    },

    // Credential encryption key
    credentialSecret: process.env.NODE_RED_CREDENTIAL_SECRET || "motorhand-secret-key-change-in-production",

    /*******************************************************************************
     * Editor Settings
     ******************************************************************************/

    editorTheme: {
        projects: {
            // Enable projects feature for git version control
            enabled: true,
            workflow: {
                mode: "manual"
            }
        },
        page: {
            title: "MotorHandPro Node-RED",
            favicon: "/absolute/path/to/theme/icon",
            css: "/absolute/path/to/custom/css/file"
        },
        header: {
            title: "MotorHandPro Mission Control",
            image: "/absolute/path/to/header/image"
        },
        palette: {
            // Allow installation of nodes from editor
            editable: true,
            // Catalog of installable nodes
            catalogues: [
                'https://catalogue.nodered.org/catalogue.json'
            ],
            // Filter nodes by category
            allowInstall: true,
            allowUpload: true,
            allowUpdate: true,
            // Theme options
            theme: [
                {
                    category: "MotorHandPro",
                    type: "lam-api",
                    color: "#3FADB5"
                }
            ]
        },
        menu: {
            "menu-item-help": {
                label: "MotorHandPro Docs",
                url: "https://github.com/STLNFTART/MotorHandPro"
            }
        }
    },

    /*******************************************************************************
     * Node Settings
     ******************************************************************************/

    // Allow the Function node to load external modules
    functionExternalModules: true,

    // Global context store configuration
    functionGlobalContext: {
        // Environment variables accessible in function nodes
        LAM_API_URL: process.env.LAM_API_URL || 'http://host.docker.internal:8000',
        MQTT_BROKER: process.env.MQTT_BROKER || 'mqtt://mosquitto:1883',
        GITHUB_TOKEN: process.env.GITHUB_TOKEN,
        GITHUB_REPO: process.env.GITHUB_REPO || 'STLNFTART/MotorHandPro',

        // Useful libraries
        os: require('os'),
        moment: require('moment'),
        axios: require('axios')
    },

    // Export HTTP endpoints for external access
    httpNodeRoot: '/api',

    // Serve static files from this directory
    httpStatic: '/data/static',

    // Maximum HTTP request size
    apiMaxLength: '5mb',

    /*******************************************************************************
     * Context Storage
     ******************************************************************************/

    contextStorage: {
        default: {
            module: "memory"
        },
        file: {
            module: "localfilesystem",
            config: {
                dir: "/data/context",
                cache: true,
                flushInterval: 30  // Flush to disk every 30 seconds
            }
        }
    },

    /*******************************************************************************
     * Logging
     ******************************************************************************/

    logging: {
        console: {
            level: "info",
            metrics: false,
            audit: false
        },
        // Log to file as well
        file: {
            level: "info",
            metrics: true,
            audit: true,
            handler: require("./log-handler.js")
        }
    },

    /*******************************************************************************
     * Custom Settings
     ******************************************************************************/

    // Disable node deprecation warnings
    nodesDir: '/data/nodes',

    // User directory for persistence
    userDir: '/data',

    // Flow file settings
    flowFile: 'flows.json',
    flowFilePretty: true,

    // Safeguard against deploy replacing all flows
    credentialsFile: 'flows_cred.json',

    /*******************************************************************************
     * Performance Settings
     ******************************************************************************/

    // Maximum number of messages per second to process
    nodeMessageBufferMaxLength: 0,  // 0 = unlimited

    // Maximum number of re-throws for node errors
    maxThreadRetry: 3,

    /*******************************************************************************
     * MQTT Settings
     ******************************************************************************/

    // MQTT broker settings (for MQTT nodes)
    // Note: Individual MQTT nodes should reference environment variables

    /*******************************************************************************
     * Storage Settings
     ******************************************************************************/

    // Storage module
    storageModule: require("node-red/lib/storage/localfilesystem"),

    /*******************************************************************************
     * Custom Node Paths
     ******************************************************************************/

    nodesExcludes: [
        'node_modules/**',
        '90-exec.js'  // Disable exec node for security
    ],

    /*******************************************************************************
     * Feature Flags
     ******************************************************************************/

    disableEditor: false,
    httpAdminRoot: '/',

    // Enable CORS for HTTP endpoints
    httpNodeCors: {
        origin: "*",
        methods: "GET,PUT,POST,DELETE"
    }
};
