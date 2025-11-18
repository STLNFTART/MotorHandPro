#!/bin/bash
# Create MQTT password file
# Default users: motorhand (full access), grafana (read-only)

echo "Creating MQTT password file..."

# Install mosquitto_passwd if not available
if ! command -v mosquitto_passwd &> /dev/null; then
    echo "mosquitto_passwd not found. Run: apt-get install mosquitto-clients"
    echo "Using docker to generate passwords..."
    docker run -it --rm -v $(pwd):/temp eclipse-mosquitto:2 \
        mosquitto_passwd -b /temp/passwd motorhand motorhand_mqtt_password
    docker run -it --rm -v $(pwd):/temp eclipse-mosquitto:2 \
        mosquitto_passwd -b /temp/passwd grafana grafana_mqtt_password
else
    # Create password file
    mosquitto_passwd -c -b passwd motorhand motorhand_mqtt_password
    mosquitto_passwd -b passwd grafana grafana_mqtt_password
fi

echo "MQTT password file created successfully!"
echo "Default users:"
echo "  motorhand / motorhand_mqtt_password (CHANGE IN PRODUCTION!)"
echo "  grafana / grafana_mqtt_password (CHANGE IN PRODUCTION!)"
