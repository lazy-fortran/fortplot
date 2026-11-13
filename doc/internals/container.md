title: Container Builds
---

# Containerized Builds

fortplot provides a Dockerfile for reproducible builds. The image uses
Ubuntu 22.04, its system compilers, and FPM 0.12.0. See `Dockerfile` for the
complete build and verification dependencies.

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

The container workflow builds and tests on the merge queue, on the default
branch after a push, and on manual dispatch. Pull requests trigger the
workflow but its build job waits for the merge queue. See
`.github/workflows/container.yml` for the current conditions.

## Publishing

The container workflow publishes its tested image to the GitHub Container
Registry on pushes to the default branch, with `latest` and commit tags.
Local builds do not publish an image.

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
