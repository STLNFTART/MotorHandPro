# LAM Docker Container
# Production-ready container for Large Action Model

FROM python:3.11-slim

# Set working directory
WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y \
    git \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Copy requirements
COPY lam/requirements.txt /app/lam/requirements.txt

# Install Python dependencies
RUN pip install --no-cache-dir -r /app/lam/requirements.txt

# Install additional dependencies for Phase 3
RUN pip install --no-cache-dir \
    fastapi \
    uvicorn[standard] \
    websockets \
    python-multipart \
    SpeechRecognition \
    pyttsx3 \
    numpy

# Copy application code
COPY lam/ /app/lam/
COPY extras/primal/primal_constants.py /app/extras/primal/primal_constants.py

# Create data directories
RUN mkdir -p /data/lam/config /data/lam/logs /data/lam/experiments

# Set environment variables
ENV PYTHONPATH=/app
ENV LAM_DATA_DIR=/data/lam
ENV LAM_LOG_LEVEL=INFO

# Expose ports
EXPOSE 8000 8080

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD python -c "import sys; sys.path.insert(0, '/app'); from lam.core.primal_lam import PrimalLAM; lam = PrimalLAM(); print('OK')"

# Default command - interactive mode
CMD ["python", "-u", "lam/lam_main.py"]
