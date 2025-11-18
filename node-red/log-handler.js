/**
 * Custom log handler for Node-RED
 * Writes logs to file with rotation
 */

const fs = require('fs');
const path = require('path');

// Log directory
const logDir = process.env.LOG_DIR || '/data/logs';

// Ensure log directory exists
if (!fs.existsSync(logDir)) {
    fs.mkdirSync(logDir, { recursive: true });
}

// Current log file
const logFile = path.join(logDir, `nodered-${new Date().toISOString().split('T')[0]}.log`);

module.exports = function(settings) {
    return {
        log: function(msg) {
            const timestamp = new Date().toISOString();
            const logLine = `${timestamp} [${msg.level}] [${msg.type}] ${msg.msg}\n`;

            fs.appendFile(logFile, logLine, (err) => {
                if (err) {
                    console.error('Error writing to log file:', err);
                }
            });
        }
    };
};
