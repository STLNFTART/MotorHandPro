/*
 * MotorHandPro - Arduino Prosthetic Hand Controller
 * Real-time EMG signal acquisition and motor control
 *
 * Hardware Requirements:
 * - Arduino Mega 2560 or compatible (for 16 analog inputs)
 * - 8x EMG sensors (MyoWare or similar)
 * - 5x Servo motors (SG90 or MG996R)
 * - Power supply (6V 2A recommended)
 *
 * Pin Configuration:
 * - A0-A7: EMG sensor inputs
 * - D2-D6: Servo PWM outputs (thumb, index, middle, ring, pinky)
 * - D13: Status LED
 *
 * Communication:
 * - Serial @ 115200 baud
 * - JSON protocol for LAM integration
 */

#include <Servo.h>

// EMG Configuration
#define NUM_EMG_CHANNELS 8
#define SAMPLE_RATE 1000  // Hz
#define BUFFER_SIZE 200   // 200ms window at 1000Hz

// Servo Configuration
#define NUM_SERVOS 5
const int SERVO_PINS[] = {2, 3, 4, 5, 6};  // thumb, index, middle, ring, pinky

// EMG sensor pins
const int EMG_PINS[] = {A0, A1, A2, A3, A4, A5, A6, A7};

// Servo objects
Servo fingers[NUM_SERVOS];

// EMG data buffer
int emgBuffer[NUM_EMG_CHANNELS][BUFFER_SIZE];
int bufferIndex = 0;

// Timing
unsigned long lastSampleTime = 0;
unsigned long sampleInterval = 1000000 / SAMPLE_RATE;  // microseconds

// Status
bool streamingEnabled = false;
int radiationDose = 0;  // Simulated radiation dose in mSv

// Gesture positions (degrees)
const int GESTURE_POSITIONS[][NUM_SERVOS] = {
  {0, 0, 0, 0, 0},       // rest (open)
  {90, 90, 90, 90, 90},  // hand_close
  {45, 0, 0, 0, 0},      // pinch_grip
  {90, 90, 90, 90, 90},  // power_grip
  {0, 90, 0, 0, 0},      // pointing
  {90, 0, 0, 0, 90}      // peace_sign
};

void setup() {
  Serial.begin(115200);

  // Initialize servos
  for (int i = 0; i < NUM_SERVOS; i++) {
    fingers[i].attach(SERVO_PINS[i]);
    fingers[i].write(0);  // Start open
  }

  // Initialize EMG pins
  for (int i = 0; i < NUM_EMG_CHANNELS; i++) {
    pinMode(EMG_PINS[i], INPUT);
  }

  // Status LED
  pinMode(13, OUTPUT);
  digitalWrite(13, LOW);

  // Send ready signal
  Serial.println("{\"status\":\"ready\",\"version\":\"1.0\"}");
}

void loop() {
  unsigned long currentTime = micros();

  // Sample EMG at fixed rate
  if (currentTime - lastSampleTime >= sampleInterval) {
    lastSampleTime = currentTime;
    sampleEMG();
  }

  // Process serial commands
  if (Serial.available() > 0) {
    processCommand();
  }
}

void sampleEMG() {
  // Read all EMG channels
  for (int ch = 0; ch < NUM_EMG_CHANNELS; ch++) {
    int value = analogRead(EMG_PINS[ch]);
    emgBuffer[ch][bufferIndex] = value;
  }

  bufferIndex = (bufferIndex + 1) % BUFFER_SIZE;

  // Stream data if enabled (decimated to avoid serial overflow)
  if (streamingEnabled && bufferIndex % 10 == 0) {
    streamEMGData();
  }
}

void streamEMGData() {
  Serial.print("{\"emg\":[");
  for (int ch = 0; ch < NUM_EMG_CHANNELS; ch++) {
    if (ch > 0) Serial.print(",");
    Serial.print(emgBuffer[ch][bufferIndex]);
  }
  Serial.print("],\"time\":");
  Serial.print(millis());
  Serial.println("}");
}

void processCommand() {
  String cmd = Serial.readStringUntil('\n');
  cmd.trim();

  // Simple JSON parsing (for production, use ArduinoJson library)
  if (cmd.startsWith("{\"cmd\":\"start_stream\"")) {
    streamingEnabled = true;
    digitalWrite(13, HIGH);
    Serial.println("{\"status\":\"streaming\"}");

  } else if (cmd.startsWith("{\"cmd\":\"stop_stream\"")) {
    streamingEnabled = false;
    digitalWrite(13, LOW);
    Serial.println("{\"status\":\"stopped\"}");

  } else if (cmd.startsWith("{\"cmd\":\"gesture\"")) {
    // Extract gesture type
    int gestureStart = cmd.indexOf("\"type\":\"") + 8;
    int gestureEnd = cmd.indexOf("\"", gestureStart);
    String gesture = cmd.substring(gestureStart, gestureEnd);
    executeGesture(gesture);

  } else if (cmd.startsWith("{\"cmd\":\"get_buffer\"")) {
    sendBufferData();

  } else if (cmd.startsWith("{\"cmd\":\"set_radiation\"")) {
    // Extract radiation dose
    int doseStart = cmd.indexOf("\"dose\":") + 7;
    int doseEnd = cmd.indexOf(",", doseStart);
    if (doseEnd == -1) doseEnd = cmd.indexOf("}", doseStart);
    radiationDose = cmd.substring(doseStart, doseEnd).toInt();

    // Simulate radiation effects on sensors
    applyRadiationNoise();

    Serial.print("{\"status\":\"radiation_set\",\"dose\":");
    Serial.print(radiationDose);
    Serial.println("}");

  } else if (cmd.startsWith("{\"cmd\":\"calibrate\"")) {
    calibrateSensors();

  } else if (cmd.startsWith("{\"cmd\":\"status\"")) {
    sendStatus();
  }
}

void executeGesture(String gesture) {
  int gestureIndex = 0;

  if (gesture == "hand_close") gestureIndex = 1;
  else if (gesture == "pinch_grip") gestureIndex = 2;
  else if (gesture == "power_grip") gestureIndex = 3;
  else if (gesture == "pointing") gestureIndex = 4;
  else if (gesture == "peace_sign") gestureIndex = 5;

  // Move servos to gesture position
  for (int i = 0; i < NUM_SERVOS; i++) {
    fingers[i].write(GESTURE_POSITIONS[gestureIndex][i]);
  }

  Serial.print("{\"status\":\"gesture_executed\",\"gesture\":\"");
  Serial.print(gesture);
  Serial.println("\"}");
}

void sendBufferData() {
  // Send EMG buffer data for LAM processing
  Serial.println("{\"buffer\":{");

  for (int ch = 0; ch < NUM_EMG_CHANNELS; ch++) {
    Serial.print("\"ch");
    Serial.print(ch);
    Serial.print("\":[");

    for (int i = 0; i < BUFFER_SIZE; i++) {
      if (i > 0) Serial.print(",");
      Serial.print(emgBuffer[ch][i]);
    }

    Serial.print("]");
    if (ch < NUM_EMG_CHANNELS - 1) Serial.print(",");
  }

  Serial.println("}}");
}

void applyRadiationNoise() {
  // Simulate radiation-induced noise and degradation
  if (radiationDose > 0) {
    float noiseFactor = 1.0 + (radiationDose / 1000.0);

    // Add noise to EMG readings
    for (int ch = 0; ch < NUM_EMG_CHANNELS; ch++) {
      for (int i = 0; i < BUFFER_SIZE; i++) {
        int noise = random(-5, 6) * noiseFactor;
        emgBuffer[ch][i] = constrain(emgBuffer[ch][i] + noise, 0, 1023);
      }
    }
  }
}

void calibrateSensors() {
  Serial.println("{\"status\":\"calibrating\"}");

  // Collect baseline data for 2 seconds
  unsigned long startTime = millis();
  long baselines[NUM_EMG_CHANNELS] = {0};
  int sampleCount = 0;

  while (millis() - startTime < 2000) {
    for (int ch = 0; ch < NUM_EMG_CHANNELS; ch++) {
      baselines[ch] += analogRead(EMG_PINS[ch]);
    }
    sampleCount++;
    delay(10);
  }

  // Calculate averages
  Serial.print("{\"calibration\":[");
  for (int ch = 0; ch < NUM_EMG_CHANNELS; ch++) {
    if (ch > 0) Serial.print(",");
    Serial.print(baselines[ch] / sampleCount);
  }
  Serial.println("]}");
}

void sendStatus() {
  Serial.print("{\"status\":{");
  Serial.print("\"streaming\":");
  Serial.print(streamingEnabled ? "true" : "false");
  Serial.print(",\"radiation_dose\":");
  Serial.print(radiationDose);
  Serial.print(",\"buffer_index\":");
  Serial.print(bufferIndex);
  Serial.print(",\"uptime\":");
  Serial.print(millis());
  Serial.println("}}");
}
