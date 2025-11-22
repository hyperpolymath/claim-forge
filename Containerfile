# Claim Forge - Container Image
# IP registration and timestamping system

FROM fedora:latest

LABEL maintainer="Hyperpolymath"
LABEL description="Claim Forge - IP registration and timestamping system"
LABEL version="1.0.0"

# Install dependencies
RUN dnf update -y && \
    dnf install -y \
        gcc-gnat \
        gprbuild \
        make \
        git \
        gnupg2 \
        python3 \
        python3-pip \
        && \
    dnf clean all

# Install OpenTimestamps client
RUN pip3 install --no-cache-dir opentimestamps-client

# Create application directory
WORKDIR /app

# Copy source code
COPY src/ /app/src/
COPY Makefile /app/
COPY claim-forge.gpr /app/

# Build the application
RUN make clean && make release

# Create directory for claims
RUN mkdir -p /workspace

# Set working directory for user operations
WORKDIR /workspace

# Configure Git (user must override with their own details)
RUN git config --global user.name "Claim Forge User" && \
    git config --global user.email "user@example.com" && \
    git config --global commit.gpgsign false

# Add the binary to PATH
ENV PATH="/app/bin:${PATH}"

# Volume for workspace
VOLUME ["/workspace"]

# Default command
ENTRYPOINT ["/app/bin/claim-forge"]
CMD ["--help"]

# Usage:
# Build: podman build -t claim-forge .
# Run: podman run -it --rm -v $(pwd):/workspace claim-forge
# Interactive: podman run -it --rm -v $(pwd):/workspace claim-forge /bin/bash
