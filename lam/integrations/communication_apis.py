"""
Communication API Executors
Slack, Discord, Telegram, Twilio, SendGrid, Mailchimp, etc.
"""

from typing import Dict, Any, List, Optional
from .api_framework import RESTAPIExecutor, api_registry
import hmac
import hashlib


@api_registry.register
class SlackExecutor(RESTAPIExecutor):
    """Slack API integration"""

    @property
    def service_name(self) -> str:
        return "slack"

    @property
    def base_url(self) -> str:
        return "https://slack.com/api"

    def _get_headers(self) -> Dict[str, str]:
        headers = super()._get_headers()
        if "bot_token" in self.credentials:
            headers["Authorization"] = f"Bearer {self.credentials['bot_token']}"
        return headers

    async def execute(self, operation: str, **kwargs) -> Dict[str, Any]:
        """Execute Slack operation"""
        if operation == "send_message":
            return await self.send_message(
                channel=kwargs.get("channel"),
                text=kwargs.get("text"),
                blocks=kwargs.get("blocks")
            )
        elif operation == "send_file":
            return await self.send_file(
                channel=kwargs.get("channel"),
                file_path=kwargs.get("file_path"),
                title=kwargs.get("title")
            )
        elif operation == "list_channels":
            return await self.list_channels()
        elif operation == "get_user_info":
            return await self.get_user_info(kwargs.get("user_id"))
        else:
            raise ValueError(f"Unknown operation: {operation}")

    async def send_message(
        self,
        channel: str,
        text: str,
        blocks: Optional[List[Dict]] = None
    ) -> Dict[str, Any]:
        """Send a message to a Slack channel"""
        data = {"channel": channel, "text": text}
        if blocks:
            data["blocks"] = blocks

        result = await self.post("chat.postMessage", data)
        self._record_execution("send_message", {"channel": channel}, result)
        return result

    async def send_file(
        self,
        channel: str,
        file_path: str,
        title: Optional[str] = None
    ) -> Dict[str, Any]:
        """Upload a file to Slack"""
        data = {"channels": channel, "file": file_path}
        if title:
            data["title"] = title

        result = await self.post("files.upload", data)
        self._record_execution("send_file", {"channel": channel, "file": file_path}, result)
        return result

    async def list_channels(self) -> Dict[str, Any]:
        """List all channels"""
        return await self.get("conversations.list")

    async def get_user_info(self, user_id: str) -> Dict[str, Any]:
        """Get user information"""
        return await self.get("users.info", user=user_id)


@api_registry.register
class DiscordExecutor(RESTAPIExecutor):
    """Discord API integration"""

    @property
    def service_name(self) -> str:
        return "discord"

    @property
    def base_url(self) -> str:
        return "https://discord.com/api/v10"

    def _get_headers(self) -> Dict[str, str]:
        headers = super()._get_headers()
        if "bot_token" in self.credentials:
            headers["Authorization"] = f"Bot {self.credentials['bot_token']}"
        return headers

    async def execute(self, operation: str, **kwargs) -> Dict[str, Any]:
        """Execute Discord operation"""
        if operation == "send_message":
            return await self.send_message(
                channel_id=kwargs.get("channel_id"),
                content=kwargs.get("content"),
                embeds=kwargs.get("embeds")
            )
        elif operation == "create_webhook":
            return await self.create_webhook(
                channel_id=kwargs.get("channel_id"),
                name=kwargs.get("name")
            )
        else:
            raise ValueError(f"Unknown operation: {operation}")

    async def send_message(
        self,
        channel_id: str,
        content: str,
        embeds: Optional[List[Dict]] = None
    ) -> Dict[str, Any]:
        """Send a message to a Discord channel"""
        data = {"content": content}
        if embeds:
            data["embeds"] = embeds

        result = await self.post(f"channels/{channel_id}/messages", data)
        self._record_execution("send_message", {"channel_id": channel_id}, result)
        return result

    async def create_webhook(self, channel_id: str, name: str) -> Dict[str, Any]:
        """Create a webhook for a channel"""
        data = {"name": name}
        return await self.post(f"channels/{channel_id}/webhooks", data)


@api_registry.register
class TelegramExecutor(RESTAPIExecutor):
    """Telegram Bot API integration"""

    @property
    def service_name(self) -> str:
        return "telegram"

    @property
    def base_url(self) -> str:
        token = self.credentials.get("bot_token", "")
        return f"https://api.telegram.org/bot{token}"

    async def execute(self, operation: str, **kwargs) -> Dict[str, Any]:
        """Execute Telegram operation"""
        if operation == "send_message":
            return await self.send_message(
                chat_id=kwargs.get("chat_id"),
                text=kwargs.get("text"),
                parse_mode=kwargs.get("parse_mode", "Markdown")
            )
        elif operation == "send_photo":
            return await self.send_photo(
                chat_id=kwargs.get("chat_id"),
                photo_url=kwargs.get("photo_url"),
                caption=kwargs.get("caption")
            )
        else:
            raise ValueError(f"Unknown operation: {operation}")

    async def send_message(
        self,
        chat_id: str,
        text: str,
        parse_mode: str = "Markdown"
    ) -> Dict[str, Any]:
        """Send a text message"""
        data = {
            "chat_id": chat_id,
            "text": text,
            "parse_mode": parse_mode
        }
        result = await self.post("sendMessage", data)
        self._record_execution("send_message", {"chat_id": chat_id}, result)
        return result

    async def send_photo(
        self,
        chat_id: str,
        photo_url: str,
        caption: Optional[str] = None
    ) -> Dict[str, Any]:
        """Send a photo"""
        data = {"chat_id": chat_id, "photo": photo_url}
        if caption:
            data["caption"] = caption

        result = await self.post("sendPhoto", data)
        self._record_execution("send_photo", {"chat_id": chat_id}, result)
        return result


@api_registry.register
class TwilioExecutor(RESTAPIExecutor):
    """Twilio API integration (SMS, Voice, WhatsApp)"""

    @property
    def service_name(self) -> str:
        return "twilio"

    @property
    def base_url(self) -> str:
        account_sid = self.credentials.get("account_sid", "")
        return f"https://api.twilio.com/2010-04-01/Accounts/{account_sid}"

    def _get_headers(self) -> Dict[str, str]:
        import base64
        headers = super()._get_headers()
        headers["Content-Type"] = "application/x-www-form-urlencoded"

        if "account_sid" in self.credentials and "auth_token" in self.credentials:
            creds = f"{self.credentials['account_sid']}:{self.credentials['auth_token']}"
            encoded = base64.b64encode(creds.encode()).decode()
            headers["Authorization"] = f"Basic {encoded}"

        return headers

    async def execute(self, operation: str, **kwargs) -> Dict[str, Any]:
        """Execute Twilio operation"""
        if operation == "send_sms":
            return await self.send_sms(
                to=kwargs.get("to"),
                from_=kwargs.get("from_"),
                body=kwargs.get("body")
            )
        elif operation == "send_whatsapp":
            return await self.send_whatsapp(
                to=kwargs.get("to"),
                from_=kwargs.get("from_"),
                body=kwargs.get("body")
            )
        elif operation == "make_call":
            return await self.make_call(
                to=kwargs.get("to"),
                from_=kwargs.get("from_"),
                url=kwargs.get("url")
            )
        else:
            raise ValueError(f"Unknown operation: {operation}")

    async def send_sms(self, to: str, from_: str, body: str) -> Dict[str, Any]:
        """Send an SMS message"""
        data = {"To": to, "From": from_, "Body": body}
        result = await self.post("Messages.json", data)
        self._record_execution("send_sms", {"to": to}, result)
        return result

    async def send_whatsapp(self, to: str, from_: str, body: str) -> Dict[str, Any]:
        """Send a WhatsApp message"""
        data = {
            "To": f"whatsapp:{to}",
            "From": f"whatsapp:{from_}",
            "Body": body
        }
        result = await self.post("Messages.json", data)
        self._record_execution("send_whatsapp", {"to": to}, result)
        return result

    async def make_call(self, to: str, from_: str, url: str) -> Dict[str, Any]:
        """Make a voice call"""
        data = {"To": to, "From": from_, "Url": url}
        return await self.post("Calls.json", data)


@api_registry.register
class SendGridExecutor(RESTAPIExecutor):
    """SendGrid Email API integration"""

    @property
    def service_name(self) -> str:
        return "sendgrid"

    @property
    def base_url(self) -> str:
        return "https://api.sendgrid.com/v3"

    def _get_headers(self) -> Dict[str, str]:
        headers = super()._get_headers()
        if "api_key" in self.credentials:
            headers["Authorization"] = f"Bearer {self.credentials['api_key']}"
        return headers

    async def execute(self, operation: str, **kwargs) -> Dict[str, Any]:
        """Execute SendGrid operation"""
        if operation == "send_email":
            return await self.send_email(
                to_email=kwargs.get("to_email"),
                from_email=kwargs.get("from_email"),
                subject=kwargs.get("subject"),
                content=kwargs.get("content"),
                content_type=kwargs.get("content_type", "text/plain")
            )
        else:
            raise ValueError(f"Unknown operation: {operation}")

    async def send_email(
        self,
        to_email: str,
        from_email: str,
        subject: str,
        content: str,
        content_type: str = "text/plain"
    ) -> Dict[str, Any]:
        """Send an email"""
        data = {
            "personalizations": [{"to": [{"email": to_email}]}],
            "from": {"email": from_email},
            "subject": subject,
            "content": [{"type": content_type, "value": content}]
        }

        result = await self.post("mail/send", data)
        self._record_execution("send_email", {"to": to_email, "subject": subject}, result)
        return result


@api_registry.register
class MailchimpExecutor(RESTAPIExecutor):
    """Mailchimp Marketing API integration"""

    @property
    def service_name(self) -> str:
        return "mailchimp"

    @property
    def base_url(self) -> str:
        # Data center is part of API key (us1, us2, etc.)
        dc = self.credentials.get("api_key", "").split("-")[-1] if "api_key" in self.credentials else "us1"
        return f"https://{dc}.api.mailchimp.com/3.0"

    def _get_headers(self) -> Dict[str, str]:
        headers = super()._get_headers()
        if "api_key" in self.credentials:
            import base64
            auth = base64.b64encode(f"anystring:{self.credentials['api_key']}".encode()).decode()
            headers["Authorization"] = f"Basic {auth}"
        return headers

    async def execute(self, operation: str, **kwargs) -> Dict[str, Any]:
        """Execute Mailchimp operation"""
        if operation == "add_subscriber":
            return await self.add_subscriber(
                list_id=kwargs.get("list_id"),
                email=kwargs.get("email"),
                merge_fields=kwargs.get("merge_fields", {})
            )
        elif operation == "send_campaign":
            return await self.send_campaign(campaign_id=kwargs.get("campaign_id"))
        else:
            raise ValueError(f"Unknown operation: {operation}")

    async def add_subscriber(
        self,
        list_id: str,
        email: str,
        merge_fields: Optional[Dict] = None
    ) -> Dict[str, Any]:
        """Add a subscriber to a list"""
        data = {
            "email_address": email,
            "status": "subscribed",
            "merge_fields": merge_fields or {}
        }

        result = await self.post(f"lists/{list_id}/members", data)
        self._record_execution("add_subscriber", {"list_id": list_id, "email": email}, result)
        return result

    async def send_campaign(self, campaign_id: str) -> Dict[str, Any]:
        """Send a campaign"""
        return await self.post(f"campaigns/{campaign_id}/actions/send", {})
