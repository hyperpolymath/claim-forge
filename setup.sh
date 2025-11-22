#!/usr/bin/env bash
# Claim Forge - System Setup Script
# Sets up development environment and dependencies

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Detect OS
detect_os() {
    if [ -f /etc/os-release ]; then
        . /etc/os-release
        OS=$ID
        OS_VERSION=$VERSION_ID
    else
        log_error "Cannot detect operating system"
        exit 1
    fi
    log_info "Detected OS: $OS $OS_VERSION"
}

# Install dependencies based on OS
install_dependencies() {
    log_info "Installing dependencies..."

    case "$OS" in
        fedora|rhel|centos)
            sudo dnf update -y
            sudo dnf install -y \
                gcc-gnat \
                gprbuild \
                make \
                git \
                gnupg2 \
                python3 \
                python3-pip
            ;;
        ubuntu|debian)
            sudo apt-get update
            sudo apt-get install -y \
                gnat \
                gprbuild \
                make \
                git \
                gnupg \
                python3 \
                python3-pip
            ;;
        arch|manjaro)
            sudo pacman -Syu --noconfirm
            sudo pacman -S --noconfirm \
                gcc-ada \
                make \
                git \
                gnupg \
                python \
                python-pip
            ;;
        *)
            log_error "Unsupported OS: $OS"
            log_warn "Please install the following manually:"
            log_warn "  - GNAT compiler (gcc-gnat/gcc-ada)"
            log_warn "  - gprbuild"
            log_warn "  - make"
            log_warn "  - git"
            log_warn "  - gnupg2"
            log_warn "  - python3 and pip3"
            exit 1
            ;;
    esac

    log_info "System dependencies installed successfully"
}

# Install Python dependencies
install_python_deps() {
    log_info "Installing Python dependencies..."
    pip3 install --user opentimestamps-client
    log_info "OpenTimestamps client installed"
}

# Check if GPG key exists
check_gpg_key() {
    log_info "Checking GPG configuration..."

    if gpg --list-secret-keys | grep -q "sec"; then
        log_info "GPG key found"
        gpg --list-secret-keys --keyid-format LONG
    else
        log_warn "No GPG key found"
        log_info "To create a new GPG key, run:"
        log_info "  gpg --full-generate-key"
        log_info ""
        log_info "Then configure Git to use it:"
        log_info "  git config --global user.signingkey YOUR_KEY_ID"
        log_info "  git config --global commit.gpgsign true"
    fi
}

# Configure Git
configure_git() {
    log_info "Checking Git configuration..."

    if [ -z "$(git config --global user.name || true)" ]; then
        log_warn "Git user.name not set"
        read -p "Enter your name: " git_name
        git config --global user.name "$git_name"
    fi

    if [ -z "$(git config --global user.email || true)" ]; then
        log_warn "Git user.email not set"
        read -p "Enter your email: " git_email
        git config --global user.email "$git_email"
    fi

    log_info "Git configured for: $(git config --global user.name) <$(git config --global user.email)>"
}

# Build the project
build_project() {
    log_info "Building Claim Forge..."

    if [ ! -f "Makefile" ]; then
        log_error "Makefile not found. Are you in the project directory?"
        exit 1
    fi

    make clean
    make release

    if [ -f "bin/claim-forge" ]; then
        log_info "Build successful: bin/claim-forge"
    else
        log_error "Build failed"
        exit 1
    fi
}

# Verify installation
verify_installation() {
    log_info "Verifying installation..."

    # Check GNAT
    if command -v gnat >/dev/null 2>&1; then
        log_info "GNAT: $(gnat --version | head -n1)"
    else
        log_error "GNAT not found"
        return 1
    fi

    # Check Git
    if command -v git >/dev/null 2>&1; then
        log_info "Git: $(git --version)"
    else
        log_error "Git not found"
        return 1
    fi

    # Check GPG
    if command -v gpg >/dev/null 2>&1; then
        log_info "GPG: $(gpg --version | head -n1)"
    else
        log_error "GPG not found"
        return 1
    fi

    # Check OpenTimestamps
    if command -v ots >/dev/null 2>&1; then
        log_info "OpenTimestamps: $(ots --version || echo 'installed')"
    else
        log_warn "OpenTimestamps client not found in PATH"
    fi

    log_info "Verification complete"
}

# Main setup flow
main() {
    echo "========================================="
    echo "  Claim Forge - Setup Script"
    echo "========================================="
    echo ""

    detect_os

    read -p "Install system dependencies? (y/N) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        install_dependencies
    fi

    read -p "Install Python dependencies? (y/N) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        install_python_deps
    fi

    configure_git
    check_gpg_key

    read -p "Build the project now? (y/N) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        build_project
    fi

    verify_installation

    echo ""
    log_info "Setup complete!"
    echo ""
    log_info "Next steps:"
    log_info "  1. Ensure you have a GPG key configured"
    log_info "  2. Run 'make' to build the project"
    log_info "  3. Run './bin/claim-forge --help' to get started"
    echo ""
}

# Run main function
main "$@"
