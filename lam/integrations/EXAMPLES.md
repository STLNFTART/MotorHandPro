# API Integration Examples

## Communication APIs

### Slack Integration

```python
from lam.integrations import get_api

# Initialize Slack
slack = get_api("slack", credentials={"bot_token": "xoxb-your-token"})

# Send simple message
await slack.execute("send_message",
    channel="#general",
    text="🚀 LAM system online!"
)

# Send with rich blocks
await slack.execute("send_message",
    channel="#alerts",
    text="Health Alert",
    blocks=[{
        "type": "section",
        "text": {
            "type": "mrkdwn",
            "text": "*Health Score:* 85%\n*Status:* Warning"
        }
    }]
)
```

### Discord Integration

```python
from lam.integrations import get_api

# Initialize Discord
discord = get_api("discord", credentials={"bot_token": "your-bot-token"})

# Send message with embed
await discord.execute("send_message",
    channel_id="123456789",
    content="Satellite analysis complete",
    embeds=[{
        "title": "Constellation Status",
        "description": "All satellites operational",
        "color": 0x00FF00,
        "fields": [
            {"name": "Active", "value": "49876", "inline": True},
            {"name": "Coverage", "value": "98.5%", "inline": True}
        ]
    }]
)
```

### Telegram Integration

```python
from lam.integrations import get_api

# Initialize Telegram
telegram = get_api("telegram", credentials={"bot_token": "your-bot-token"})

# Send message
await telegram.execute("send_message",
    chat_id="@yourchannel",
    text="*LAM Alert*\n\nSystem health: 🟢 Healthy",
    parse_mode="Markdown"
)

# Send photo with caption
await telegram.execute("send_photo",
    chat_id="123456789",
    photo_url="https://example.com/chart.png",
    caption="Latest performance metrics"
)
```

### Twilio SMS/WhatsApp

```python
from lam.integrations import get_api

# Initialize Twilio
twilio = get_api("twilio", credentials={
    "account_sid": "ACxxxxx",
    "auth_token": "your-auth-token"
})

# Send SMS
await twilio.execute("send_sms",
    to="+1234567890",
    from_="+0987654321",
    body="Critical alert: System health below threshold"
)

# Send WhatsApp message
await twilio.execute("send_whatsapp",
    to="+1234567890",
    from_="+0987654321",
    body="Your satellite analysis is ready!"
)
```

### SendGrid Email

```python
from lam.integrations import get_api

# Initialize SendGrid
sendgrid = get_api("sendgrid", credentials={"api_key": "SG.xxxxx"})

# Send email
await sendgrid.execute("send_email",
    to_email="alerts@example.com",
    from_email="noreply@motorhandpro.com",
    subject="Daily Satellite Sweep Results",
    content="All 5 configurations passed stability checks.",
    content_type="text/plain"
)

# Send HTML email
await sendgrid.execute("send_email",
    to_email="team@example.com",
    from_email="system@motorhandpro.com",
    subject="Weekly LAM Report",
    content="<h1>Performance Summary</h1><p>Success rate: 98.5%</p>",
    content_type="text/html"
)
```

## Integration with LAM Actions

### Example: Send Slack notification after trip planning

```python
# In lam_main.py or action executor
async def plan_trip_with_notification(destination, departure, return_date):
    # Plan trip
    result = lam.plan_trip(destination, departure, return_date)

    # Send Slack notification
    slack = get_api("slack", credentials={"bot_token": os.getenv("SLACK_BOT_TOKEN")})
    await slack.execute("send_message",
        channel="#travel",
        text=f"✈️ Trip to {destination} planned!\nDeparture: {departure}\nReturn: {return_date}"
    )

    return result
```

### Example: Discord alert on resonance field instability

```python
# In primal_lam.py
async def check_stability_and_alert():
    rf = quantum_resonance_field.get_state()

    if not rf['stable']:
        discord = get_api("discord", credentials={"bot_token": os.getenv("DISCORD_BOT_TOKEN")})
        await discord.execute("send_message",
            channel_id=os.getenv("DISCORD_ALERT_CHANNEL"),
            content="⚠️ **Resonance Field Unstable**",
            embeds=[{
                "title": "System Alert",
                "color": 0xFF0000,
                "fields": [
                    {"name": "Alpha", "value": str(rf['alpha']), "inline": True},
                    {"name": "Lambda", "value": str(rf['lambda']), "inline": True},
                    {"name": "Lipschitz", "value": str(rf['lipschitz_constant']), "inline": True}
                ]
            }]
        )
```

## Node-RED Integration

### Flow: Satellite Sweep → Slack Notification

```json
[
    {
        "id": "satellite_sweep",
        "type": "inject",
        "repeat": "3600",
        "payload": ""
    },
    {
        "id": "lam_analyze",
        "type": "lam-api-call",
        "endpoint": "/ask",
        "method": "POST"
    },
    {
        "id": "format_slack",
        "type": "function",
        "func": "msg.service = 'slack';\nmsg.operation = 'send_message';\nmsg.payload = {\n    channel: '#satellite-ops',\n    text: `Sweep complete: ${msg.results.summary}`\n};\nreturn msg;"
    },
    {
        "id": "api_executor",
        "type": "api-executor",
        "name": "Send to Slack"
    }
]
```

### Flow: MQTT Telemetry → Discord Alert

```javascript
// Function node: Check telemetry
if (msg.payload.altitude < 100) {
    msg.service = "discord";
    msg.operation = "send_message";
    msg.payload = {
        channel_id: process.env.DISCORD_CHANNEL,
        content: "🚨 Low altitude warning!",
        embeds: [{
            title: "Drone Alert",
            description: `Altitude: ${msg.payload.altitude}m`,
            color: 0xFF0000
        }]
    };
    return msg;
}
return null;
```

## Webhook Receivers

### Slack Webhook Handler

```python
from fastapi import Request
from lam.integrations.communication_apis import SlackExecutor

@app.post("/webhooks/slack")
async def slack_webhook(request: Request):
    payload = await request.body()
    signature = request.headers.get("X-Slack-Signature")

    # Verify signature
    slack = SlackExecutor(credentials={"signing_secret": os.getenv("SLACK_SIGNING_SECRET")})
    if not await slack.verify_signature(payload, signature):
        raise HTTPException(status_code=401, detail="Invalid signature")

    data = await request.json()

    # Handle event
    if data.get("type") == "event_callback":
        event = data["event"]
        if event["type"] == "message":
            # Process incoming message
            await process_slack_message(event["text"], event["channel"])

    return {"status": "ok"}
```

## Complete Example: Multi-Channel Alert System

```python
from lam.integrations import get_api
import asyncio

async def send_multi_channel_alert(message: str, severity: str):
    """Send alert to multiple channels based on severity"""

    # Prepare tasks
    tasks = []

    if severity in ["warning", "critical"]:
        # Send to Slack
        slack = get_api("slack", credentials={"bot_token": os.getenv("SLACK_BOT_TOKEN")})
        tasks.append(slack.execute("send_message",
            channel="#alerts",
            text=f"⚠️ {severity.upper()}: {message}"
        ))

    if severity == "critical":
        # Send SMS via Twilio
        twilio = get_api("twilio", credentials={
            "account_sid": os.getenv("TWILIO_SID"),
            "auth_token": os.getenv("TWILIO_TOKEN")
        })
        tasks.append(twilio.execute("send_sms",
            to=os.getenv("ADMIN_PHONE"),
            from_=os.getenv("TWILIO_PHONE"),
            body=f"CRITICAL: {message}"
        ))

        # Send email via SendGrid
        sendgrid = get_api("sendgrid", credentials={"api_key": os.getenv("SENDGRID_API_KEY")})
        tasks.append(sendgrid.execute("send_email",
            to_email=os.getenv("ADMIN_EMAIL"),
            from_email="alerts@motorhandpro.com",
            subject=f"CRITICAL ALERT: {message}",
            content=f"<h1>Critical System Alert</h1><p>{message}</p>",
            content_type="text/html"
        ))

    # Execute all notifications concurrently
    await asyncio.gather(*tasks, return_exceptions=True)
```

## Using with LAM Webhooks

```python
from lam.api.webhook_manager import webhook_manager
from lam.integrations import get_api

# When LAM detects health issue
async def on_health_critical(health_data):
    # Emit webhook event (for Node-RED, etc.)
    await webhook_manager.emit("health.critical", health_data)

    # Also send direct notifications
    slack = get_api("slack", credentials={"bot_token": os.getenv("SLACK_BOT_TOKEN")})
    await slack.execute("send_message",
        channel="#ops",
        text=f"🚨 Health Critical: {health_data['health_score']}%"
    )
```

## Environment Variables Setup

```bash
# .env file
SLACK_BOT_TOKEN=xoxb-your-token-here
DISCORD_BOT_TOKEN=your-discord-bot-token
TELEGRAM_BOT_TOKEN=your-telegram-bot-token
TWILIO_SID=ACxxxxxxxxxxxxx
TWILIO_TOKEN=your-twilio-token
TWILIO_PHONE=+1234567890
SENDGRID_API_KEY=SG.xxxxxxx
MAILCHIMP_API_KEY=xxxxxxxxx-us1
```

## Error Handling Best Practices

```python
from lam.integrations import get_api
import logging

logger = logging.getLogger(__name__)

async def safe_api_call(service: str, operation: str, **kwargs):
    """Safely execute API call with error handling"""
    try:
        executor = get_api(service, credentials=load_credentials(service))
        result = await executor.execute(operation, **kwargs)
        logger.info(f"{service}.{operation} succeeded")
        return {"success": True, "result": result}

    except httpx.HTTPStatusError as e:
        logger.error(f"{service}.{operation} HTTP error: {e.response.status_code}")
        return {"success": False, "error": f"HTTP {e.response.status_code}"}

    except httpx.TimeoutException:
        logger.error(f"{service}.{operation} timeout")
        return {"success": False, "error": "Timeout"}

    except Exception as e:
        logger.error(f"{service}.{operation} failed: {e}")
        return {"success": False, "error": str(e)}
```

## Rate Limiting Example

```python
from lam.integrations import get_api
import asyncio

# Batch messages to respect rate limits
async def batch_send_messages(messages: list, channel: str):
    """Send multiple messages with rate limiting"""
    slack = get_api("slack", credentials={"bot_token": os.getenv("SLACK_BOT_TOKEN")})

    for i, message in enumerate(messages):
        await slack.execute("send_message",
            channel=channel,
            text=message
        )

        # Rate limit: 1 message per second
        if i < len(messages) - 1:
            await asyncio.sleep(1)
```

---

For more examples and complete documentation, see:
- `/lam/integrations/TOP_100_APIS.md` - Full API catalog
- `/lam/integrations/api_framework.py` - Framework documentation
- `/node-red/flows/` - Node-RED integration examples
