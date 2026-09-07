# fortplot containerized build environment
# Provides reproducible builds matching CI toolchain versions
#
# Usage:
#   docker build -t fortplot .
#   docker run --rm -v $(pwd):/workspace fortplot make build
#   docker run --rm -v $(pwd):/workspace fortplot make test
#
# Publish:
#   docker buildx build --push -t ghcr.io/fortplot/fortplot:latest -t ghcr.io/fortplot/fortplot:2025.06.25 --platform linux/amd64,linux/arm64 .

FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

# Build dependencies: gfortran, gcc, cmake, ninja, ffmpeg, imagemagick, poppler-utils, ghostscript, python3, fonts
RUN apt-get update && apt-get install -y --no-install-recommends \
    gfortran \
    gcc \
    cmake \
    ninja-build \
    make \
    zlib1g-dev \
    ffmpeg \
    imagemagick \
    poppler-utils \
    ghostscript \
    python3 \
    python3-pip \
    pngcheck \
    git \
    wget \
    ca-certificates \
    fonts-liberation \
    fonts-dejavu-core \
    && rm -rf /var/lib/apt/lists/*

# Install FPM (matching CI version 0.12.0)
RUN wget -q https://github.com/fortran-lang/fpm/releases/download/v0.12.0/fpm-0.12.0-linux-x86_64-gcc-12 \
    && chmod +x fpm-0.12.0-linux-x86_64-gcc-12 \
    && mv fpm-0.12.0-linux-x86_64-gcc-12 /usr/local/bin/fpm \
    && fpm --version

# Documentation and independent rendering oracles for this build/test image.
COPY scripts/requirements-visual.txt /opt/fortplot/requirements-visual.txt
RUN pip3 install --no-cache-dir ford -r /opt/fortplot/requirements-visual.txt

# Set working directory
WORKDIR /workspace

# Default command
CMD ["bash"]
