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
 * Dependencies:
 * - ArduinoJson 7.x  (install via Library Manager: "ArduinoJson" by Benoît Blanchon)
 * - Servo.h          (bundled with the Arduino IDE)
 *
 * Pin Configuration:
 * - A0-A7: EMG sensor inputs
 * - D2-D6: Servo PWM outputs (thumb, index, middle, ring, pinky)
 * - D13: Status LED
 *
 * Communication:
 * - Serial @ 115200 baud
 * - JSON protocol for LAM integration
 *
 * Command schema (all fields required unless noted):
 *   {"cmd":"start_stream"}
 *   {"cmd":"stop_stream"}
 *   {"cmd":"gesture","type":"<name>"}   // name: rest|hand_close|pinch_grip|power_grip|pointing|peace_sign
 *   {"cmd":"set_radiation","dose":<int>} // dose in mSv (simulated)
 *   {"cmd":"get_buffer"}
 *   {"cmd":"calibrate"}
 *   {"cmd":"status"}
 *
 * Error responses use the form:
 *   {"status":"error","error":"<message>"}
 */

#include <ArduinoJson.h>
#include <Servo.h>

// EMG Configuration
#define NUM_EMG_CHANNELS 8
#define SAMPLE_RATE      1000   // Hz
#define BUFFER_SIZE      200    // 200 ms window at 1000 Hz

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
const unsigned long SAMPLE_INTERVAL_US = 1000000UL / SAMPLE_RATE;

// Status
bool streamingEnabled = false;
int  radiationDose    = 0;  // Simulated radiation dose in mSv

// Gesture positions (degrees): thumb, index, middle, ring, pinky
const int GESTURE_POSITIONS[][NUM_SERVOS] = {
  { 0,  0,  0,  0,  0},  // rest (open)
  {90, 90, 90, 90, 90},  // hand_close
  {45,  0,  0,  0,  0},  // pinch_grip
  {90, 90, 90, 90, 90},  // power_grip
  { 0, 90,  0,  0,  0},  // pointing
  {90,  0,  0,  0, 90},  // peace_sign
};

const char* GESTURE_NAMES[] = {
  "rest", "hand_close", "pinch_grip", "power_grip", "pointing", "peace_sign"
};
const int NUM_GESTURES = sizeof(GESTURE_NAMES) / sizeof(GESTURE_NAMES[0]);

// -------------------------------------------------------------------------
// Helpers
// -------------------------------------------------------------------------

void sendError(const char* message) {
  StaticJsonDocument<128> resp;
  resp["status"] = "error";
  resp["error"]  = message;
  serializeJson(resp, Serial);
  Serial.println();
}

// -------------------------------------------------------------------------
// Setup / loop
// -------------------------------------------------------------------------

void setup() {
  Serial.begin(115200);

  for (int i = 0; i < NUM_SERVOS; i++) {
    fingers[i].attach(SERVO_PINS[i]);
    fingers[i].write(0);  // Start open
  }

  for (int i = 0; i < NUM_EMG_CHANNELS; i++) {
    pinMode(EMG_PINS[i], INPUT);
  }

  pinMode(13, OUTPUT);
  digitalWrite(13, LOW);

  StaticJsonDocument<64> ready;
  ready["status"]  = "ready";
  ready["version"] = "2.0";
  serializeJson(ready, Serial);
  Serial.println();
}

void loop() {
  unsigned long currentTime = micros();

  if (currentTime - lastSampleTime >= SAMPLE_INTERVAL_US) {
    lastSampleTime = currentTime;
    sampleEMG();
  }

  if (Serial.available() > 0) {
    processCommand();
  }
}

// -------------------------------------------------------------------------
// EMG sampling
// -------------------------------------------------------------------------

void sampleEMG() {
  for (int ch = 0; ch < NUM_EMG_CHANNELS; ch++) {
    emgBuffer[ch][bufferIndex] = analogRead(EMG_PINS[ch]);
  }
  bufferIndex = (bufferIndex + 1) % BUFFER_SIZE;

  if (streamingEnabled && bufferIndex % 10 == 0) {
    streamEMGData();
  }
}

void streamEMGData() {
  // Build streaming packet without a heap allocation (print directly).
  Serial.print(F("{\"emg\":["));
  for (int ch = 0; ch < NUM_EMG_CHANNELS; ch++) {
    if (ch > 0) Serial.print(',');
    Serial.print(emgBuffer[ch][bufferIndex]);
  }
  Serial.print(F("],\"time\":"));
  Serial.print(millis());
  Serial.println('}');
}

// -------------------------------------------------------------------------
// Command parsing (ArduinoJson)
// -------------------------------------------------------------------------

void processCommand() {
  String line = Serial.readStringUntil('\n');
  line.trim();
  if (line.length() == 0) return;

  // Use a stack-allocated document sized for the largest expected command.
  StaticJsonDocument<256> doc;
  DeserializationError err = deserializeJson(doc, line);

  if (err) {
    sendError("invalid JSON");
    return;
  }

  const char* cmd = doc["cmd"];
  if (!cmd) {
    sendError("missing 'cmd' field");
    return;
  }

  if (strcmp(cmd, "start_stream") == 0) {
    streamingEnabled = true;
    digitalWrite(13, HIGH);
    StaticJsonDocument<64> resp;
    resp["status"] = "streaming";
    serializeJson(resp, Serial);
    Serial.println();

  } else if (strcmp(cmd, "stop_stream") == 0) {
    streamingEnabled = false;
    digitalWrite(13, LOW);
    StaticJsonDocument<64> resp;
    resp["status"] = "stopped";
    serializeJson(resp, Serial);
    Serial.println();

  } else if (strcmp(cmd, "gesture") == 0) {
    const char* gestureType = doc["type"];
    if (!gestureType) {
      sendError("gesture command missing 'type' field");
      return;
    }
    executeGesture(gestureType);

  } else if (strcmp(cmd, "get_buffer") == 0) {
    sendBufferData();

  } else if (strcmp(cmd, "set_radiation") == 0) {
    if (!doc.containsKey("dose")) {
      sendError("set_radiation command missing 'dose' field");
      return;
    }
    radiationDose = doc["dose"].as<int>();
    applyRadiationNoise();
    StaticJsonDocument<64> resp;
    resp["status"] = "radiation_set";
    resp["dose"]   = radiationDose;
    serializeJson(resp, Serial);
    Serial.println();

  } else if (strcmp(cmd, "calibrate") == 0) {
    calibrateSensors();

  } else if (strcmp(cmd, "status") == 0) {
    sendStatus();

  } else {
    sendError("unknown command");
  }
}

// -------------------------------------------------------------------------
// Command handlers
// -------------------------------------------------------------------------

void executeGesture(const char* gestureName) {
  int gestureIndex = -1;
  for (int i = 0; i < NUM_GESTURES; i++) {
    if (strcmp(gestureName, GESTURE_NAMES[i]) == 0) {
      gestureIndex = i;
      break;
    }
  }

  if (gestureIndex < 0) {
    sendError("unknown gesture type");
    return;
  }

  for (int i = 0; i < NUM_SERVOS; i++) {
    fingers[i].write(GESTURE_POSITIONS[gestureIndex][i]);
  }

  StaticJsonDocument<128> resp;
  resp["status"]  = "gesture_executed";
  resp["gesture"] = gestureName;
  serializeJson(resp, Serial);
  Serial.println();
}

void sendBufferData() {
  // Build the buffer JSON manually to avoid large heap allocation.
  Serial.print(F("{\"buffer\":{"));
  for (int ch = 0; ch < NUM_EMG_CHANNELS; ch++) {
    if (ch > 0) Serial.print(',');
    Serial.print('"');
    Serial.print(F("ch"));
    Serial.print(ch);
    Serial.print(F("\":["));
    for (int i = 0; i < BUFFER_SIZE; i++) {
      if (i > 0) Serial.print(',');
      Serial.print(emgBuffer[ch][i]);
    }
    Serial.print(']');
  }
  Serial.println(F("}}"));
}

void applyRadiationNoise() {
  if (radiationDose <= 0) return;

  float noiseFactor = 1.0f + (radiationDose / 1000.0f);
  for (int ch = 0; ch < NUM_EMG_CHANNELS; ch++) {
    for (int i = 0; i < BUFFER_SIZE; i++) {
      int noise = random(-5, 6) * (int)noiseFactor;
      emgBuffer[ch][i] = constrain(emgBuffer[ch][i] + noise, 0, 1023);
    }
  }
}

void calibrateSensors() {
  StaticJsonDocument<64> starting;
  starting["status"] = "calibrating";
  serializeJson(starting, Serial);
  Serial.println();

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

  // Emit calibration result as a JSON array.
  Serial.print(F("{\"calibration\":["));
  for (int ch = 0; ch < NUM_EMG_CHANNELS; ch++) {
    if (ch > 0) Serial.print(',');
    Serial.print(sampleCount > 0 ? baselines[ch] / sampleCount : 0);
  }
  Serial.println(F("]}"));
}

void sendStatus() {
  StaticJsonDocument<128> resp;
  JsonObject s      = resp.createNestedObject("status");
  s["streaming"]    = streamingEnabled;
  s["radiation_dose"] = radiationDose;
  s["buffer_index"] = bufferIndex;
  s["uptime"]       = millis();
  serializeJson(resp, Serial);
  Serial.println();
}
