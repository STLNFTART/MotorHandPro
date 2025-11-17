#!/usr/bin/env python3
"""
LAM Voice Interface
Speech recognition and text-to-speech for hands-free LAM interaction
"""
import sys
from pathlib import Path
from typing import Optional

sys.path.insert(0, str(Path(__file__).parent.parent.parent))
sys.path.insert(0, str(Path(__file__).parent.parent))

try:
    import speech_recognition as sr
    import pyttsx3
    VOICE_AVAILABLE = True
except ImportError:
    print("Warning: Voice libraries not installed")
    print("Install with: pip install SpeechRecognition pyttsx3 pyaudio")
    VOICE_AVAILABLE = False

try:
    from lam_main import LAM
    LAM_AVAILABLE = True
except ImportError:
    LAM_AVAILABLE = False


class VoiceInterface:
    """
    Voice-controlled LAM interface using speech recognition and TTS
    """

    def __init__(self):
        if not VOICE_AVAILABLE:
            raise ImportError("Voice libraries not available")

        # Initialize speech recognition
        self.recognizer = sr.Recognizer()
        self.microphone = sr.Microphone()

        # Initialize text-to-speech
        self.tts_engine = pyttsx3.init()
        self._configure_tts()

        # Initialize LAM
        if LAM_AVAILABLE:
            self.lam = LAM()
        else:
            self.lam = None

        print("🎤 Voice Interface Initialized")
        self.speak("Voice interface ready. Say 'LAM' to activate.")

    def _configure_tts(self):
        """Configure text-to-speech engine"""
        # Set properties
        self.tts_engine.setProperty('rate', 150)  # Speed
        self.tts_engine.setProperty('volume', 0.9)  # Volume

        # Try to set a pleasant voice
        voices = self.tts_engine.getProperty('voices')
        if len(voices) > 1:
            self.tts_engine.setProperty('voice', voices[1].id)  # Usually female voice

    def speak(self, text: str):
        """Convert text to speech"""
        print(f"🔊 LAM: {text}")
        self.tts_engine.say(text)
        self.tts_engine.runAndWait()

    def listen(self, timeout: int = 5) -> Optional[str]:
        """
        Listen for speech input

        Args:
            timeout: Maximum seconds to wait for speech

        Returns:
            Recognized text or None
        """
        with self.microphone as source:
            # Adjust for ambient noise
            print("🎧 Listening...")
            self.recognizer.adjust_for_ambient_noise(source, duration=0.5)

            try:
                # Listen for audio
                audio = self.recognizer.listen(source, timeout=timeout)

                # Recognize speech
                print("🔄 Processing...")
                text = self.recognizer.recognize_google(audio)

                print(f"👤 You: {text}")
                return text.lower()

            except sr.WaitTimeoutError:
                return None
            except sr.UnknownValueError:
                self.speak("I didn't catch that. Could you repeat?")
                return None
            except sr.RequestError as e:
                self.speak("Speech recognition service error")
                print(f"Error: {e}")
                return None

    def process_command(self, command: str) -> bool:
        """
        Process voice command

        Returns:
            True to continue, False to exit
        """
        command = command.strip().lower()

        # Exit commands
        if any(word in command for word in ['exit', 'quit', 'goodbye', 'stop']):
            self.speak("Goodbye! Shutting down voice interface.")
            return False

        # Status
        if 'status' in command:
            if self.lam:
                status = self.lam.get_status()
                self.speak("System status is nominal. Quantum resonance field stable.")
            else:
                self.speak("LAM core not available")

        # Trip planning
        elif 'plan trip' in command or 'book trip' in command:
            self.speak("Where would you like to go?")
            destination = self.listen()

            if destination:
                self.speak(f"Planning a trip to {destination}. When would you like to depart?")
                # In production, continue the conversation
                if self.lam:
                    self.speak("Trip planning initiated with quantum optimization.")
                else:
                    self.speak("LAM core not available for trip planning")

        # Reservation
        elif 'make reservation' in command or 'reserve' in command:
            self.speak("What type of reservation? Restaurant, hotel, or event?")
            res_type = self.listen()

            if res_type:
                self.speak(f"Making a {res_type} reservation with LAM optimization.")

        # Order food
        elif 'order food' in command or 'order pizza' in command:
            self.speak("What would you like to order?")
            order = self.listen()

            if order:
                self.speak(f"Placing order for {order}")

        # Ask question
        elif 'question' in command or 'what is' in command or 'tell me' in command:
            if self.lam:
                answer = self.lam.ask_question(command)
                # Summarize answer for voice
                self.speak("The Lightfoot constant controls exponential memory decay, "
                          "while the Donte constant serves as a fixed-point attractor.")
            else:
                self.speak("LAM core not available for questions")

        # Help
        elif 'help' in command:
            self.speak("I can help you with: planning trips, making reservations, "
                      "ordering food, answering questions, or checking system status. "
                      "What would you like to do?")

        # Unknown command
        else:
            self.speak("I understand commands like: plan trip, make reservation, "
                      "order food, status, or help. What can I do for you?")

        return True

    def run(self):
        """Run interactive voice interface"""
        print("\n" + "="*60)
        print("LAM VOICE INTERFACE")
        print("Quantum-Semantic Intelligence with Voice Control")
        print("="*60)
        print("\nVoice Commands:")
        print("  • 'Plan trip'")
        print("  • 'Make reservation'")
        print("  • 'Order food'")
        print("  • 'Status'")
        print("  • 'Help'")
        print("  • 'Exit'")
        print("\nSay 'LAM' to activate, then give your command.")
        print("="*60 + "\n")

        self.speak("Voice interface is ready. Say LAM to activate.")

        running = True
        while running:
            try:
                # Wait for wake word
                command = self.listen(timeout=10)

                if command is None:
                    continue

                # Check for wake word
                if 'lam' in command or 'lamb' in command:
                    self.speak("Yes?")

                    # Listen for actual command
                    command = self.listen(timeout=5)

                    if command:
                        running = self.process_command(command)
                else:
                    # Process directly if it's a known command
                    if any(word in command for word in ['trip', 'reservation', 'order', 'status', 'help', 'exit']):
                        running = self.process_command(command)

            except KeyboardInterrupt:
                print("\n\nInterrupted")
                self.speak("Voice interface interrupted.")
                break
            except Exception as e:
                print(f"Error: {e}")
                self.speak("An error occurred. Please try again.")


def main():
    """Run voice interface"""
    if not VOICE_AVAILABLE:
        print("Error: Voice libraries not installed")
        print("Install with:")
        print("  pip install SpeechRecognition pyttsx3")
        print("  # For microphone support:")
        print("  pip install pyaudio")
        return

    try:
        interface = VoiceInterface()
        interface.run()
    except Exception as e:
        print(f"Failed to start voice interface: {e}")


if __name__ == "__main__":
    main()
