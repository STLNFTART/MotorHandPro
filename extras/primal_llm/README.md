# Primal LLM - AI-Assisted Control System Interface

Natural language interface for Primal Logic control systems using Large Language Models (LLMs).

## Overview

**Primal LLM** provides conversational AI capabilities for:
- Natural language control system queries
- AI-assisted parameter tuning
- Voice-controlled system configuration
- Intelligent diagnostic assistance
- Educational explanations of Primal Logic concepts

⚠️ **EXPERIMENTAL** - Research prototype for exploring human-AI collaboration in control system engineering.

## Features

### Natural Language Queries

**Examples:**
```
User: "What is the current Lipschitz constant?"
AI: "The Lipschitz constant F'(D) is currently 0.000129931830, which is safely below 1.0, guaranteeing system stability."

User: "Why is my system oscillating?"
AI: "Oscillations may indicate KE is too high. Try reducing KE from 0.5 to 0.3 for better damping. Would you like me to adjust it?"

User: "Explain exponential memory weighting in simple terms"
AI: "Think of it like gradually forgetting old mistakes. Recent errors matter more (100% weight), but errors from 6 seconds ago only matter about 37%. This prevents the system from 'holding grudges' about past problems."
```

### AI-Assisted Tuning

**Interactive parameter optimization:**
```
User: "I need faster response without overshoot"
AI: "I recommend:
     1. Increase λ from 0.16905 to 0.25 (faster decay)
     2. Keep KE at 0.3 (conservative gain)
     3. Monitor Ec(t) for stability
     Shall I apply these changes?"

User: "Yes, apply and test"
AI: [Applies parameters, runs simulation]
    "New settling time: 4.2s (was 5.9s)
     Overshoot: 1.8% (acceptable)
     Stability: PASS ✓"
```

### Voice Interaction

**Voice commands:** (Requires WebSpeech API support)
```
User: [speaks] "Show me the phase portrait"
AI: [displays 3D phase space visualization]

User: [speaks] "Set lambda to point two"
AI: "Setting λ to 0.2. This increases decay rate by 18%. Confirm?"

User: [speaks] "Confirm"
AI: "Applied. System is stable with new parameters."
```

### Real-Time Diagnostics

**Intelligent monitoring:**
```
AI: ⚠️ "Warning: Ec(t) increasing faster than expected. Possible causes:
     1. External disturbance detected
     2. KE may be too high
     3. Plant parameters may have changed
     Recommendation: Enable adaptive mode or reduce KE to 0.2"

User: "What's causing the disturbance?"
AI: "Analyzing sensor data... Frequency signature matches mechanical vibration at 2.3 Hz. Suggest adding notch filter or adjusting mounting."
```

## Files

### Main Interface

- **`PrimalLLM.html`** - Primary web interface (latest version)
  - Chat interface with conversation history
  - Parameter control panel integration
  - Real-time visualization embedding
  - Voice input/output controls

- **`index.html`** - Alternative interface (backup/testing)

### Backup Versions

- **`PrimalLLM.backup`** - Stable fallback version
- **`PrimalLLM.backup2`** - Previous iteration
- **`PrimalLLM.backup3`** - Early prototype

### Maintenance Scripts

- **`fix_primal_llm.sh`** - Repair common issues
  - Fixes JavaScript syntax errors
  - Restores corrupted HTML
  - Validates API connections

- **`fix_syntax.sh`** - Automated syntax validation
  - ESLint checks
  - HTML validation
  - CSS formatting

- **`clean_useeffect.sh`** - React cleanup utilities
  - Removes duplicate useEffect hooks
  - Optimizes component re-renders

### Development Files

- **`clean_parts.txt`** - Code snippets and templates
  - Reusable UI components
  - API integration examples
  - Prompt engineering templates

## Installation & Usage

### Quick Start

```bash
cd extras/primal_llm

# Option 1: Direct browser access
firefox PrimalLLM.html
# or
google-chrome PrimalLLM.html

# Option 2: Local web server (recommended)
python -m http.server 8080
# Navigate to: http://localhost:8080/PrimalLLM.html
```

### LLM Backend Configuration

**Primal LLM supports multiple LLM backends:**

#### 1. OpenAI API

```javascript
// In PrimalLLM.html, configure:
const LLM_CONFIG = {
  provider: 'openai',
  api_key: 'your-api-key-here',  // Store securely!
  model: 'gpt-4',
  temperature: 0.7
};
```

#### 2. Local LLM (Ollama, LM Studio)

```javascript
const LLM_CONFIG = {
  provider: 'local',
  endpoint: 'http://localhost:11434/api/generate',
  model: 'llama2',
  temperature: 0.7
};
```

#### 3. Claude API

```javascript
const LLM_CONFIG = {
  provider: 'anthropic',
  api_key: 'your-api-key-here',
  model: 'claude-3-sonnet-20240229',
  temperature: 0.7
};
```

### Connecting to MotorHandPro

**WebSocket connection to live system:**

```javascript
// In PrimalLLM.html
const MOTORHAND_CONFIG = {
  websocket_url: 'ws://localhost:8765',  // LAM system WebSocket
  serial_port: '/dev/ttyACM0',           // Arduino serial
  update_rate: 100  // Hz
};
```

**Connection status:**
- 🟢 Green: Connected and receiving data
- 🟡 Yellow: Connected but no data
- 🔴 Red: Disconnected

## Features in Detail

### Conversational Parameter Tuning

**AI understands context:**

```
User: "The system is too slow"
AI: "I'll help speed it up. Current settling time is 5.9s.
     Options:
     A) Increase λ to 0.25 (moderate speedup, safe)
     B) Increase KE to 0.5 (aggressive, may overshoot)
     C) Both (fastest, requires testing)
     Which approach?"

User: "Start with A"
AI: "Applied λ = 0.25. New settling time: ~4.0s.
     Testing stability... ✓ F'(D) = 0.00019 < 1.0
     System remains stable. Want to try option B for further improvement?"
```

### Explain Mode

**Educational responses:**

```
User: "Explain what D represents"
AI: "D (the Donte constant, 149.9992314000) is the fixed point of the Primal Logic kernel.

     Think of it as the 'natural equilibrium' of the system - like a marble settling at the bottom of a bowl. No matter where the system starts, it will eventually converge toward this value.

     Mathematically, it's where F(D) = D, meaning the function maps to itself.

     In control terms, it represents the theoretical steady-state with zero error and no external inputs.

     Want me to show the iterative convergence visually?"
```

### Safety Guardrails

**AI prevents dangerous configurations:**

```
User: "Set KE to 10.0"
AI: "⚠️ Warning: KE = 10.0 is extremely high and will likely cause instability.

     Analysis:
     - Predicted overshoot: >200%
     - Risk of actuator saturation
     - Lipschitz condition may be violated

     Maximum safe KE for current λ: ~0.8

     Do you want to:
     A) Set KE to 0.8 (maximum safe value)
     B) Proceed anyway (UNSAFE - simulation only)
     C) Cancel"
```

## API Integration

### JavaScript API

```javascript
// Send query to LLM
async function askPrimalAI(question) {
  const response = await fetch('/api/llm/query', {
    method: 'POST',
    headers: {'Content-Type': 'application/json'},
    body: JSON.stringify({
      question: question,
      context: getCurrentSystemState()
    })
  });
  return await response.json();
}

// Example usage
const answer = await askPrimalAI("Why is Ec increasing?");
console.log(answer.explanation);
```

### System State Context

**AI receives full system context:**

```javascript
function getCurrentSystemState() {
  return {
    parameters: {
      lambda: 0.16905,
      KE: 0.3,
      D: 149.9992314000
    },
    current_state: {
      psi: 1.0145,
      gamma: 0.0084,
      Ec: 0.0031
    },
    metrics: {
      lipschitz_constant: 0.000129931830,
      settling_time: 5.92,
      stability: 'PASS'
    },
    history: last_100_samples
  };
}
```

## Prompt Engineering

### System Prompt (Hidden from User)

```
You are an expert in Primal Logic control systems. You assist users with:
1. Parameter tuning and optimization
2. Stability analysis and diagnostics
3. Educational explanations of control theory
4. Safety-critical decision support

Key principles:
- Always prioritize safety (F'(D) < 1.0 required)
- Provide reasoning for suggestions
- Warn about risks before applying changes
- Use simple language for complex concepts
- Cite equations when relevant

Constants to remember:
- λ (Lightfoot constant) = 0.16905 s⁻¹
- D (Donte constant) = 149.9992314000
- F'(D) must be < 1.0 for stability
- τ = 1/λ ≈ 5.92s (time constant)
```

### User Prompt Templates

**Diagnostic template:**
```
System status:
- psi(t) = {psi}
- gamma(t) = {gamma}
- Ec(t) = {Ec}
- Behavior: {observed_behavior}

Question: What's wrong and how do I fix it?
```

**Tuning template:**
```
Goal: {user_goal}
Current performance:
- Settling time: {t_settle}
- Overshoot: {overshoot}%
- Stability: {stability_status}

Suggest parameter adjustments.
```

## Voice Control

### Supported Commands

**Parameter control:**
- "Set lambda to [value]"
- "Increase KE by [amount]"
- "Reset to default parameters"

**Visualization:**
- "Show phase portrait"
- "Display time series"
- "Plot Ec over time"

**Diagnostics:**
- "Check stability"
- "Analyze performance"
- "What's the current state?"

**System control:**
- "Start simulation"
- "Stop system"
- "Emergency stop"

### Voice Configuration

```javascript
// Enable voice recognition
const voiceConfig = {
  enabled: true,
  language: 'en-US',
  continuous: false,  // Push-to-talk
  interim_results: true
};

// Voice synthesis settings
const ttsConfig = {
  voice: 'Google US English',
  rate: 1.0,
  pitch: 1.0,
  volume: 1.0
};
```

## Troubleshooting

### Common Issues

**1. LLM not responding:**
```bash
# Check API key
echo $OPENAI_API_KEY

# Test connection
curl https://api.openai.com/v1/models \
  -H "Authorization: Bearer $OPENAI_API_KEY"
```

**2. Voice input not working:**
- Requires HTTPS or localhost
- Check browser permissions (microphone access)
- Supported browsers: Chrome, Edge (WebSpeech API)

**3. WebSocket connection failed:**
```bash
# Verify LAM system running
ps aux | grep lam_orchestrator

# Check WebSocket port
netstat -an | grep 8765

# Restart LAM system
cd .. && python lam_orchestrator.py
```

**4. Syntax errors:**
```bash
# Run repair script
./fix_primal_llm.sh

# Validate HTML
./fix_syntax.sh
```

## Security Considerations

⚠️ **IMPORTANT SECURITY NOTES:**

1. **API Keys:**
   - Never commit API keys to version control
   - Use environment variables or config files (git-ignored)
   - Rotate keys regularly

2. **Local Use Only:**
   - Not designed for public internet deployment
   - No authentication/authorization
   - Assumes trusted local network

3. **LLM Hallucinations:**
   - **Always validate AI suggestions before applying**
   - Check stability conditions (F'(D) < 1.0)
   - Monitor Ec(t) after parameter changes
   - Use simulation mode before hardware deployment

4. **Data Privacy:**
   - System state sent to external LLM APIs
   - May include proprietary control parameters
   - Consider local LLM for sensitive applications

## Future Enhancements

- [ ] Multi-modal input (images, sensor plots)
- [ ] Fine-tuned Primal Logic-specific model
- [ ] Reinforcement learning from human feedback (RLHF)
- [ ] Integration with control_panel/ web UI
- [ ] Mobile app voice assistant
- [ ] Offline mode with local LLM
- [ ] Multi-language support
- [ ] Video tutorials generated by AI

## Related Documentation

- [Control Panel](../../control_panel/README.md) - Web visualization interface
- [LAM System](../../lam/README.md) - Backend orchestration
- [Primal Logic Framework](../../PRIMAL_LOGIC_FRAMEWORK.md) - Theoretical foundations

---

**Patent Pending:** U.S. Provisional Patent Application No. 63/842,846

**Research Note:** Primal LLM explores human-AI collaboration in safety-critical control systems. Use responsibly and validate all AI-generated suggestions.
