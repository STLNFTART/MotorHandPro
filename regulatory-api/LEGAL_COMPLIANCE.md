# Legal & Compliance Requirements for Regulatory API Integration

**CRITICAL: READ BEFORE DEPLOYMENT**

This document outlines the legal, compliance, and Terms of Service (ToS) requirements for using public regulatory APIs. **Failure to comply can result in API access termination, legal liability, or regulatory penalties.**

---

## Table of Contents

1. [General Principles](#general-principles)
2. [FDA / openFDA](#fda--openfda)
3. [NHTSA](#nhtsa)
4. [FAA](#faa)
5. [What You CAN Automate](#what-you-can-automate)
6. [What You CANNOT Automate](#what-you-cannot-automate)
7. [Data Usage & Liability](#data-usage--liability)
8. [Rate Limiting & Fair Use](#rate-limiting--fair-use)
9. [PHI/PII Handling](#phipii-handling)
10. [Compliance Checklist](#compliance-checklist)

---

## General Principles

### Do's ✅

- **Respect rate limits** and fair use policies
- **Attribute data sources** properly in outputs
- **Cache responsibly** to reduce API load
- **Use API keys** when available (reduces limits, shows good faith)
- **Read and comply with ToS** for each API
- **Treat all data as "advisory only"** – not authoritative for safety-critical decisions without independent verification

### Don'ts ❌

- **Do NOT use for clinical/medical decision-making** without proper validation and professional oversight
- **Do NOT hammer APIs** with excessive requests
- **Do NOT redistribute bulk data** without checking licensing terms
- **Do NOT automate regulatory submissions** without explicit approval and legal agreements
- **Do NOT scrape/crawl** – use official APIs only
- **Do NOT claim data is real-time or complete** unless explicitly stated by the source

---

## FDA / openFDA

### API Overview

- **Service**: openFDA
- **URL**: https://open.fda.gov/
- **Authentication**: Optional API key (highly recommended)
- **Rate Limits**:
  - **Without key**: 240 requests/minute, 120,000 requests/day
  - **With key**: 240 requests/minute, **unlimited** requests/day

### Terms of Service

- **ToS**: https://open.fda.gov/terms/
- **Key points**:
  - Data provided "as-is" with no warranties
  - **NOT for clinical use** without proper validation
  - **NOT a substitute for professional medical advice**
  - FDA reserves the right to limit or terminate access
  - Attribution recommended: "Data provided by the U.S. Food and Drug Administration"

### What This Means for You

✅ **ALLOWED**:
- Querying recall data for research, simulation validation, context enrichment
- Searching adverse event reports to compare with simulated failure modes
- Aggregating data for internal analysis

❌ **NOT ALLOWED** without proper safeguards:
- Using adverse event data to make clinical recommendations
- Claiming your system "detects FDA violations"
- Automated reporting to FDA systems (eCTD, etc.) without proper agreements

### Recommendations

1. **Get an API key**: https://open.fda.gov/apis/authentication/
2. **Add disclaimers** to any output that includes FDA data:
   ```
   "This data is sourced from openFDA and is provided for informational purposes only.
   It should not be used for medical decision-making without independent verification
   and consultation with qualified professionals."
   ```
3. **Cache aggressively** – data doesn't change minute-to-minute
4. **Monitor your usage** via the rate limit info endpoint

---

## NHTSA

### API Overview

- **Service**: NHTSA vPIC (Product Information Catalog) + Safety APIs
- **URLs**:
  - vPIC: https://vpic.nhtsa.dot.gov/api/
  - Safety: https://api.nhtsa.gov/
- **Authentication**: None required
- **Rate Limits**: Not explicitly published, but be respectful (recommend max 60 req/min)

### Terms of Service

- **General ToS**: https://www.nhtsa.gov/privacy-policy
- **Key points**:
  - Public data, free to use
  - Data provided "as-is"
  - **NOT for safety-critical decisions** without verification
  - NHTSA does not endorse third-party use

### What This Means for You

✅ **ALLOWED**:
- Decoding VINs for vehicle identification
- Querying recall data for AV/vehicle platforms
- Searching complaints and safety ratings
- Enriching simulation data with real-world vehicle specs

❌ **NOT ALLOWED**:
- Automating defect reporting without proper process
- Claiming NHTSA endorsement of your simulations
- Using recall data as sole source for safety compliance decisions

### Recommendations

1. **Implement rate limiting** on your end (60 req/min max)
2. **Cache VIN decodes and static vehicle data** – this rarely changes
3. **Add disclaimers**:
   ```
   "Recall and safety data sourced from NHTSA. This information is provided for
   reference only and should be independently verified for safety-critical applications."
   ```
4. **Don't abuse the APIs** – NHTSA can block IPs

---

## FAA

### API Overview

- **Service**: AviationWeather.gov + FAA Data Portal
- **URLs**:
  - Weather: https://aviationweather.gov/data/api/
  - Data Portal: https://www.faa.gov/air_traffic/flight_info/aeronav/digital_products/
- **Authentication**: Some endpoints require API key registration
- **Rate Limits**: Not explicitly published, but be respectful

### Terms of Service

- **ToS**: https://www.aviationweather.gov/data/api/
- **Key points**:
  - Data is **NOT for flight planning** without proper validation
  - Weather data is **observational and forecasted** – subject to change
  - **NOT suitable for navigation** without official sources
  - FAA reserves right to limit access

### What This Means for You

✅ **ALLOWED**:
- Pulling METAR/TAF/SIGMET/AIRMET data for simulation context
- Correlating simulated UAV/aircraft operations with real weather regimes
- Researching historical weather patterns
- Using PIREPs for qualitative analysis

❌ **NOT ALLOWED**:
- Using as sole source for actual flight planning or navigation
- Claiming FAA approval/endorsement
- Automating airspace filings (Form 7460, OE/AAA) without proper process
- Treating weather data as authoritative for safety-of-flight decisions

### Recommendations

1. **Register for an API key** if planning heavy use
2. **Cache weather data** with reasonable TTL (15-30 minutes for METAR, 6 hours for TAF)
3. **Add disclaimers**:
   ```
   "Weather data sourced from AviationWeather.gov. This data is for informational
   purposes only and is NOT suitable for flight planning, navigation, or safety-of-flight
   decisions without verification from official sources."
   ```
4. **Implement retries with exponential backoff** for transient failures
5. **Don't poll continuously** – weather updates every 1-6 hours depending on product type

---

## What You CAN Automate

These are **low-friction, safe uses** of public regulatory APIs:

### ✅ Data Retrieval & Context Enrichment

- **Search** for recalls, adverse events, complaints, weather
- **Decode** VINs, look up vehicle specs
- **Retrieve** historical regulatory data for comparison with simulations
- **Aggregate** data for internal dashboards and reports
- **Correlate** simulation outputs with real-world regulatory cases

### ✅ Internal Research & Analysis

- Building datasets for ML training (subject to licensing)
- Comparative analysis (e.g., "How does our sim compare to real recalls?")
- Risk assessment inputs (with proper caveats)

### ✅ Notification & Monitoring

- Weekly cron jobs to check for new recalls on tracked platforms
- Alerting when new adverse events match your failure mode patterns
- Monitoring regulatory trends

---

## What You CANNOT Automate

These require **human-in-the-loop** and often **legal agreements**:

### ❌ Regulatory Submissions

- **FDA eCTD** submissions (drug/device applications)
- **FDA MAUDE** reporting (device adverse events)
- **NHTSA TREAD** reporting (vehicle defects)
- **FAA Form 7460** (obstruction evaluation)
- **FAA Part 107 waivers** (UAS operations)

**Why**: These systems have strict formats, legal liability, authentication, and often require organizational vetting.

### ❌ Safety-Critical Decisions

- Using API data as **sole source** for clinical decisions
- Using API data as **sole source** for vehicle safety certifications
- Using API data as **sole source** for flight planning or navigation

**Why**: Data is often delayed, incomplete, or requires professional interpretation.

### ❌ Commercial Redistribution

- Selling raw FDA/NHTSA/FAA data without proper licensing
- Claiming endorsement by regulatory agencies

**Why**: ToS restrictions and legal liability.

---

## Data Usage & Liability

### Disclaimers You MUST Include

For **any** system output that incorporates regulatory API data:

```
REGULATORY DATA DISCLAIMER

This system incorporates data from the following U.S. government sources:
- U.S. Food and Drug Administration (FDA) via openFDA
- National Highway Traffic Safety Administration (NHTSA)
- Federal Aviation Administration (FAA) via AviationWeather.gov

This data is provided "as-is" for informational and research purposes only.
It is NOT suitable for:
- Clinical or medical decision-making
- Safety-critical engineering decisions (without independent verification)
- Flight planning or navigation
- Regulatory compliance certification (without professional review)

Users must independently verify all regulatory data before making decisions
that affect health, safety, or compliance. The creators of this system make
no warranties regarding the accuracy, completeness, or timeliness of
regulatory data.

For official guidance, consult:
- FDA: https://www.fda.gov
- NHTSA: https://www.nhtsa.gov
- FAA: https://www.faa.gov
```

### Liability Waiver

**You are responsible for**:
- Understanding and complying with each agency's ToS
- Properly disclaiming data limitations
- Not making safety-critical decisions solely based on API data
- Protecting any PHI/PII (see below)

---

## Rate Limiting & Fair Use

### Recommended Limits (Your Side)

Even if APIs don't publish explicit limits, **implement your own**:

| API | Requests/Minute | Requests/Day | Notes |
|-----|----------------|--------------|-------|
| openFDA (no key) | 240 | 120,000 | Get a key for unlimited daily |
| openFDA (with key) | 240 | Unlimited | Still respect per-minute limit |
| NHTSA | 60 | 10,000 | Conservative estimate |
| FAA Weather | 60 | 5,000 | Weather updates slowly |

### Caching Strategy

- **Recalls/Adverse Events**: Cache for 24 hours (data updates daily)
- **VIN Decodes**: Cache indefinitely (static data)
- **METAR**: Cache for 30 minutes (updates hourly)
- **TAF**: Cache for 6 hours (updates every 6 hours)
- **Safety Ratings**: Cache indefinitely (static for a given model year)

### Retry Logic

- **Max retries**: 3
- **Backoff**: Exponential (2s, 4s, 8s)
- **Don't retry** on 400/401/403 (client errors)
- **Do retry** on 429/500/502/503 (rate limit, server errors)

---

## PHI/PII Handling

### What is PHI/PII?

- **PHI** (Protected Health Information): Medical records, patient identifiers, etc.
- **PII** (Personally Identifiable Information): Names, addresses, SSNs, etc.

### Regulatory APIs and PHI/PII

- **openFDA**: Adverse event reports are **de-identified** but may contain indirect identifiers. Treat as **potentially sensitive**.
- **NHTSA**: Complaints may contain **PII** (names, addresses). Handle carefully.
- **FAA**: Generally no PHI/PII in weather/obstruction data.

### Your Obligations

If you **ever** touch PHI/PII:

1. **HIPAA Compliance** (if applicable):
   - You may need a Business Associate Agreement (BAA)
   - Implement encryption (data at rest and in transit)
   - Access controls and audit logs
   - Breach notification procedures

2. **Do NOT**:
   - Store PHI/PII without encryption
   - Transmit over unencrypted channels
   - Share with third parties without consent
   - Use for purposes beyond what's legally allowed

3. **For this project**:
   - We are **NOT** storing patient-level data
   - We are using **aggregated, de-identified** adverse event data for research
   - **Still**: Implement proper security practices

---

## Compliance Checklist

Before deploying this system, ensure:

### Technical

- [ ] Rate limiting implemented on your side
- [ ] Exponential backoff retry logic in place
- [ ] API keys configured (FDA at minimum)
- [ ] Caching strategy implemented
- [ ] Error handling for all API calls
- [ ] Logging of API usage for audit trail

### Legal

- [ ] Read and understood each API's ToS
- [ ] Disclaimers added to all outputs using regulatory data
- [ ] No claims of regulatory endorsement
- [ ] No automated submissions to regulatory systems without approval
- [ ] PHI/PII handling procedures in place (if applicable)

### Operational

- [ ] Monitoring for API health and rate limits
- [ ] Alerts for quota exhaustion or repeated errors
- [ ] Documentation for users on data limitations
- [ ] Process for updating when ToS changes

### Testing

- [ ] Tested all endpoints with sample queries
- [ ] Verified rate limiting works
- [ ] Confirmed error handling gracefully degrades
- [ ] Validated disclaimer text appears in outputs

---

## Contact & Support

If you have questions about compliance:

- **FDA openFDA**: https://open.fda.gov/about/contact/
- **NHTSA**: https://www.nhtsa.gov/about-nhtsa/contact-us
- **FAA**: https://www.faa.gov/contact/

**For legal advice**: Consult a lawyer specializing in regulatory compliance.

---

**Last Updated**: 2025-11-15
**Review Frequency**: Quarterly (or when ToS changes)
**Maintained By**: MotorHandPro Regulatory Integration Team
