#!/usr/bin/env python3
import hashlib
import datetime

class PrimalChatBot:
    def __init__(self, auth_token):
        self.auth_token = auth_token
        self.active = False
        self.memory_log = []

    def primal_start(self, token):
        if token == self.auth_token:
            self.active = True
            self.log_event("PRIMAL_START authorized.")
            print("✅ Primal Chat Bot activated.")
        else:
            print("❌ Unauthorized start signal. Silence enforced.")

    def primal_stop(self):
        self.active = False
        self.log_event("PRIMAL_STOP issued.")
        print("🛑 Primal Chat Bot deactivated.")

    def log_event(self, event):
        timestamp = datetime.datetime.now().isoformat()
        signature = hashlib.sha256((event + timestamp).encode()).hexdigest()
        self.memory_log.append({"event": event, "timestamp": timestamp, "sig": signature})

    def process_input(self, user_input):
        if not self.active:
            print("🤐 Primal Silence: Bot is inactive.")
            return

        if not self.verify_input(user_input):
            print("🤐 Primal Silence: Data insufficient or unauthorized.")
            self.log_event("Rejected input.")
            return

        response = self.generate_response(user_input)
        print(response)
        self.log_event(f"Processed input: {user_input}")

    def verify_input(self, user_input):
        keywords = ["truth", "verify"]
        return any(keyword in user_input.lower() for keyword in keywords)

    def generate_response(self, user_input):
        return f"🔹 Primal Response: Verified input accepted — '{user_input}'."

# -------------------------------
# Run the bot
# -------------------------------
if __name__ == "__main__":
    primal_token = "12345678"  # <-- set your secret token here

    bot = PrimalChatBot(auth_token=primal_token)

    print("🔰 Welcome to Primal Chat Bot demo.")
    token_input = input("Enter activation token: ")
    bot.primal_start(token=token_input)

    while bot.active:
        user_message = input("\nYou: ")
        if user_message.lower() == "stop":
            bot.primal_stop()

