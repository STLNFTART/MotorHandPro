# LAM Mobile App

React Native mobile application for LAM (Large Action Model).

## Features

- Trip planning with quantum optimization
- Restaurant reservations
- Food ordering
- Voice commands
- Q&A with LAM
- System status monitoring

## Setup

### Prerequisites

```bash
# Install Node.js 16+
# Install React Native CLI
npm install -g react-native-cli

# For iOS (macOS only)
sudo gem install cocoapods
cd LAMApp/ios && pod install

# For Android
# Install Android Studio and SDK
```

### Installation

```bash
cd LAMApp
npm install
```

### Configuration

Set the LAM API URL in `.env`:

```
LAM_API_URL=http://your-server:8000
```

### Running

```bash
# iOS
npm run ios

# Android
npm run android

# Start Metro bundler
npm start
```

## Architecture

```
LAMApp/
├── src/
│   ├── api/
│   │   └── lam_client.js     # API client
│   ├── screens/
│   │   ├── HomeScreen.js
│   │   ├── TripScreen.js
│   │   ├── ReservationScreen.js
│   │   ├── OrderScreen.js
│   │   └── StatusScreen.js
│   ├── components/
│   │   └── ...
│   └── navigation/
│       └── AppNavigator.js
├── android/
├── ios/
└── package.json
```

## Voice Commands

The app supports voice commands:
- "Plan a trip to Paris"
- "Make a reservation"
- "Order pizza"
- "System status"

## Development

```bash
# Run tests
npm test

# Lint code
npm run lint

# Build for production
# iOS
cd ios && xcodebuild
# Android
cd android && ./gradlew assembleRelease
```

## Deployment

### iOS

1. Open `ios/LAMApp.xcworkspace` in Xcode
2. Configure signing
3. Archive and upload to App Store

### Android

1. Generate signed APK:
```bash
cd android
./gradlew assembleRelease
```

2. Upload to Google Play Console

## License

Patent Pending: U.S. Provisional Patent Application No. 63/842,846
© 2025 Donte Lightfoot
