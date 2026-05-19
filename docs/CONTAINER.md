---
title: Containerized Builds
---

# Containerized Builds

fortplot provides a Dockerfile for reproducible builds. The container matches
the toolchain versions used in CI (Ubuntu 22.04, gfortran, FPM 0.12.0).

## Quick Start

```bash
# Build the image
docker build -t fortplot .

# Build the project
docker run --rm -v $(pwd):/workspace fortplot make build

# Run tests
docker run --rm -v $(pwd):/workspace fortplot make test

# Run examples
docker run --rm -v $(pwd):/workspace fortplot make example
```

## Interactive Shell

```bash
docker run --rm -it -v $(pwd):/workspace fortplot bash
```

## CI Integration

The containerized build runs in CI on every push and pull request. See
`.github/workflows/container.yml` for the workflow definition.

## Publishing

The image is published to GHCR on pushes to main:

```bash
docker buildx build \
  --push \
  -t ghcr.io/fortplot/fortplot:latest \
  -t ghcr.io/fortplot/fortplot:2025.06.25 \
  --platform linux/amd64,linux/arm64 \
  .
```

## Toolchain

| Tool        | Version     |
|-------------|-------------|
| Ubuntu      | 22.04       |
| gfortran    | (system)    |
| gcc         | (system)    |
| cmake       | (system)    |
| ninja       | (system)    |
| FPM         | 0.12.0      |
| ffmpeg      | (system)    |
| Python3     | (system)    |
| FORD        | (pip)       |
