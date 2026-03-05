FROM mcr.microsoft.com/devcontainers/rust:1-1-bullseye

# Install tools required for scripts and benchmarking
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends \
    jq \
    bc \
    awk \
    python3 \
    python3-pip \
    && apt-get clean -y \
    && rm -rf /var/lib/apt/lists/*