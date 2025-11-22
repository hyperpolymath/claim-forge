#!/usr/bin/env bash
# Claim Forge - Monitoring Script
# Monitors claim registration processes and system health

set -euo pipefail

# Configuration
LOG_DIR="/var/log/claim-forge"
LOG_FILE="$LOG_DIR/monitor.log"
PID_FILE="/var/run/claim-forge-monitor.pid"
CHECK_INTERVAL=60  # seconds

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Logging function
log() {
    local level=$1
    shift
    local message="$@"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo -e "${timestamp} [${level}] ${message}" | tee -a "$LOG_FILE"
}

log_info() {
    log "INFO" "$@"
}

log_warn() {
    log "WARN" "${YELLOW}$@${NC}"
}

log_error() {
    log "ERROR" "${RED}$@${NC}"
}

log_success() {
    log "SUCCESS" "${GREEN}$@${NC}"
}

# Check if claim-forge binary exists
check_binary() {
    if command -v claim-forge &> /dev/null; then
        log_success "claim-forge binary found"
        return 0
    else
        log_error "claim-forge binary not found in PATH"
        return 1
    fi
}

# Check GPG configuration
check_gpg() {
    if ! command -v gpg &> /dev/null; then
        log_error "GPG not installed"
        return 1
    fi

    if gpg --list-secret-keys | grep -q "sec"; then
        log_success "GPG key configured"
        return 0
    else
        log_warn "No GPG secret key found"
        return 1
    fi
}

# Check OpenTimestamps client
check_ots() {
    if command -v ots &> /dev/null; then
        log_success "OpenTimestamps client found"
        return 0
    else
        log_warn "OpenTimestamps client not found"
        return 1
    fi
}

# Check Git configuration
check_git() {
    if ! command -v git &> /dev/null; then
        log_error "Git not installed"
        return 1
    fi

    if [ -n "$(git config --global user.name)" ] && [ -n "$(git config --global user.email)" ]; then
        log_success "Git configured: $(git config --global user.name) <$(git config --global user.email)>"
        return 0
    else
        log_warn "Git user.name or user.email not configured"
        return 1
    fi
}

# Check disk space
check_disk_space() {
    local threshold=90
    local usage=$(df -h / | tail -1 | awk '{print $5}' | sed 's/%//')

    if [ "$usage" -lt "$threshold" ]; then
        log_success "Disk usage: ${usage}%"
        return 0
    else
        log_warn "Disk usage high: ${usage}%"
        return 1
    fi
}

# Monitor claim files
monitor_claims() {
    local claim_count=$(find . -name "CLAIM.md" 2>/dev/null | wc -l)
    local ots_count=$(find . -name "*.ots" 2>/dev/null | wc -l)
    local sig_count=$(find . -name "*.sig" 2>/dev/null | wc -l)

    log_info "Claims found: $claim_count, Timestamps: $ots_count, Signatures: $sig_count"
}

# Verify recent timestamps
verify_recent_timestamps() {
    log_info "Checking recent OpenTimestamps proofs..."

    # Find .ots files modified in the last 24 hours
    while IFS= read -r ots_file; do
        if [ -f "$ots_file" ]; then
            log_info "Verifying: $ots_file"
            if ots verify "$ots_file" &> /dev/null; then
                log_success "✓ $ots_file verified"
            else
                log_warn "⚠ $ots_file not yet confirmed (may need more blockchain confirmations)"
            fi
        fi
    done < <(find . -name "*.ots" -mtime -1 2>/dev/null)
}

# System health check
health_check() {
    log_info "==== Claim Forge Health Check ===="

    local all_ok=true

    check_binary || all_ok=false
    check_gpg || all_ok=false
    check_ots || all_ok=false
    check_git || all_ok=false
    check_disk_space || all_ok=false

    monitor_claims
    verify_recent_timestamps

    if $all_ok; then
        log_success "All health checks passed"
        return 0
    else
        log_warn "Some health checks failed"
        return 1
    fi
}

# Continuous monitoring mode
monitor_mode() {
    log_info "Starting continuous monitoring (interval: ${CHECK_INTERVAL}s)"
    log_info "Press Ctrl+C to stop"

    # Write PID file
    echo $$ > "$PID_FILE"

    trap 'log_info "Monitoring stopped"; rm -f "$PID_FILE"; exit 0' INT TERM

    while true; do
        health_check
        echo ""
        sleep "$CHECK_INTERVAL"
    done
}

# Show help
show_help() {
    cat <<EOF
Claim Forge Monitor - System Health Monitoring

Usage: $(basename "$0") [COMMAND]

Commands:
  health     Run a single health check (default)
  monitor    Run continuous monitoring
  check      Check specific component (gpg|ots|git|disk)
  verify     Verify recent timestamps
  help       Show this help message

Examples:
  $(basename "$0")              # Run health check once
  $(basename "$0") monitor      # Continuous monitoring
  $(basename "$0") check gpg    # Check GPG only
  $(basename "$0") verify       # Verify timestamps

Logs: $LOG_FILE
EOF
}

# Main
main() {
    # Create log directory if it doesn't exist
    mkdir -p "$LOG_DIR" 2>/dev/null || true

    case "${1:-health}" in
        health)
            health_check
            ;;
        monitor)
            monitor_mode
            ;;
        check)
            case "${2:-}" in
                gpg) check_gpg ;;
                ots) check_ots ;;
                git) check_git ;;
                disk) check_disk_space ;;
                *) log_error "Unknown component: ${2:-}"; show_help; exit 1 ;;
            esac
            ;;
        verify)
            verify_recent_timestamps
            ;;
        help|-h|--help)
            show_help
            ;;
        *)
            log_error "Unknown command: $1"
            show_help
            exit 1
            ;;
    esac
}

main "$@"
