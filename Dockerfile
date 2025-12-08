# MotorHandPro Multi-Service Base Image
# Supports: motorhand-api, motorhand-regulatory
# Patent Pending: U.S. Provisional Patent Application No. 63/842,846

FROM python:3.11-slim AS base

LABEL maintainer="Donte Lightfoot <donte@primaltechinvest.com>"
LABEL description="MotorHandPro LAM + Primal Logic API"
LABEL version="1.0.0"

# Set working directory
WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y \
    git \
    build-essential \
    curl \
    wget \
    && rm -rf /var/lib/apt/lists/*

# Copy entire repository
COPY . /app/

# Install Python dependencies
RUN pip install --no-cache-dir --upgrade pip && \
    pip install --no-cache-dir -r requirements.txt && \
    pip install --no-cache-dir -r lam_requirements.txt

# Create necessary directories
RUN mkdir -p \
    /data/lam/config \
    /data/lam/logs \
    /data/lam/experiments \
    /app/logs \
    /app/data \
    /app/output \
    /app/results \
    /app/validation_results

# Set environment variables
ENV PYTHONPATH=/app
ENV PYTHONUNBUFFERED=1
ENV LAM_DATA_DIR=/data/lam
ENV LAM_LOG_LEVEL=INFO

# Expose ports
# 8000: FastAPI (LAM API)
# 8080: WebSocket
# 9000: Regulatory API
EXPOSE 8000 8080 9000

# Health check for API service
HEALTHCHECK --interval=30s --timeout=10s --start-period=40s --retries=3 \
    CMD curl -f http://localhost:8000/health || exit 1

# Default command - can be overridden by docker-compose
CMD ["uvicorn", "lam.api.main:app", "--host", "0.0.0.0", "--port", "8000"]
