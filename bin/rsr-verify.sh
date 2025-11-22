#!/usr/bin/env bash
# RSR Compliance Verification Script
# Checks Claim Forge against Rhodium Standard Repository Framework

set -euo pipefail

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
PASS=0
FAIL=0
WARN=0

# Check functions
check_pass() {
    echo -e "${GREEN}✓${NC} $1"
    ((PASS++))
}

check_fail() {
    echo -e "${RED}✗${NC} $1"
    ((FAIL++))
}

check_warn() {
    echo -e "${YELLOW}⚠${NC} $1"
    ((WARN++))
}

echo "═══════════════════════════════════════════════"
echo "  RSR Framework Compliance Check"
echo "  Claim Forge v$(cat VERSION 2>/dev/null || echo '1.0.0')"
echo "═══════════════════════════════════════════════"
echo ""

# === Category 1: Type Safety ===
echo -e "${BLUE}[1/11] Type Safety${NC}"

if [ -f "src/claim_forge.adb" ]; then
    check_pass "Ada 2012 source files present"
else
    check_fail "Ada source files missing"
fi

if grep -q "pragma Ada_2012" src/*.ads 2>/dev/null || grep -q "gnat2012" claim-forge.gpr; then
    check_pass "Ada 2012 mode enabled"
else
    check_warn "Ada 2012 mode not explicitly set (may be default)"
fi

if grep -q "Bounded_String" src/*.ads; then
    check_pass "Bounded strings for safety"
else
    check_warn "Bounded strings pattern not detected"
fi

echo ""

# === Category 2: Memory Safety ===
echo -e "${BLUE}[2/11] Memory Safety${NC}"

if ! grep -r "new " src/*.adb 2>/dev/null | grep -v "-- " | grep -v "^$" > /dev/null; then
    check_pass "No dynamic allocation (no 'new' keyword)"
else
    check_warn "Dynamic allocation detected (check if intentional)"
fi

if [ -f "claim-forge.gpr" ] && grep -q "fstack-check" claim-forge.gpr; then
    check_pass "Stack overflow checking enabled"
else
    check_warn "Stack overflow checking not explicitly enabled"
fi

echo ""

# === Category 3: Offline-First ===
echo -e "${BLUE}[3/11] Offline-First${NC}"

if grep -q "No_Timestamp" src/cli.ads; then
    check_pass "Optional network features (--no-timestamp flag)"
else
    check_warn "Network optionality not detected in CLI"
fi

if grep -q "No_Push" src/cli.ads; then
    check_pass "Optional git push (--no-push flag)"
else
    check_warn "Git push optionality not detected"
fi

if ! grep -r "http://" src/*.adb 2>/dev/null | grep -v "-- " > /dev/null; then
    check_pass "No hardcoded HTTP URLs in source"
else
    check_fail "Hardcoded HTTP URLs found"
fi

echo ""

# === Category 4: Documentation ===
echo -e "${BLUE}[4/11] Documentation${NC}"

for file in README.md LICENSE CONTRIBUTING.md SECURITY.md CHANGELOG.md CODE_OF_CONDUCT.md MAINTAINERS.md; do
    if [ -f "$file" ]; then
        check_pass "$file present"
    else
        check_fail "$file missing"
    fi
done

echo ""

# === Category 5: .well-known/ Directory ===
echo -e "${BLUE}[5/11] .well-known/ Directory${NC}"

for file in security.txt ai.txt humans.txt; do
    if [ -f ".well-known/$file" ]; then
        check_pass ".well-known/$file present"
    else
        check_fail ".well-known/$file missing"
    fi
done

# Check RFC 9116 compliance in security.txt
if [ -f ".well-known/security.txt" ] && grep -q "Contact:" .well-known/security.txt; then
    check_pass "security.txt follows RFC 9116"
else
    check_warn "security.txt may not follow RFC 9116 format"
fi

echo ""

# === Category 6: Build System ===
echo -e "${BLUE}[6/11] Build System${NC}"

if [ -f "Makefile" ]; then
    check_pass "Makefile present"
else
    check_fail "Makefile missing"
fi

if [ -f "claim-forge.gpr" ]; then
    check_pass "GNAT project file present"
else
    check_fail "GNAT project file missing"
fi

if [ -f ".gitlab-ci.yml" ] || [ -f ".github/workflows/build.yml" ]; then
    check_pass "CI/CD pipeline configured"
else
    check_warn "CI/CD pipeline not detected"
fi

if [ -f "justfile" ]; then
    check_pass "justfile present (modern task runner)"
else
    check_warn "justfile not present (optional)"
fi

if [ -f "flake.nix" ]; then
    check_pass "Nix flake for reproducible builds"
else
    check_warn "Nix flake not present (optional, for Gold tier)"
fi

echo ""

# === Category 7: Test Coverage ===
echo -e "${BLUE}[7/11] Test Coverage${NC}"

if [ -f "docs/examples/test-workflow.sh" ]; then
    check_pass "Integration test script present"
else
    check_fail "Integration test script missing"
fi

if [ -x "docs/examples/test-workflow.sh" ]; then
    check_pass "Test script is executable"
else
    check_warn "Test script not executable"
fi

# Try running tests (if build exists)
if [ -f "bin/claim-forge" ]; then
    if ./bin/claim-forge --help > /dev/null 2>&1; then
        check_pass "Binary runs (--help test)"
    else
        check_fail "Binary fails to run"
    fi
else
    check_warn "Binary not built (run 'make' first)"
fi

echo ""

# === Category 8: TPCF (Tri-Perimeter Contribution Framework) ===
echo -e "${BLUE}[8/11] TPCF${NC}"

if [ -f "TPCF.md" ]; then
    check_pass "TPCF.md documentation present"
else
    check_fail "TPCF.md missing"
fi

if [ -f "MAINTAINERS.md" ]; then
    check_pass "MAINTAINERS.md present (perimeter documentation)"
else
    check_fail "MAINTAINERS.md missing"
fi

if grep -q "Perimeter 1" TPCF.md 2>/dev/null; then
    check_pass "Three-perimeter model documented"
else
    check_warn "TPCF perimeter structure not detected"
fi

echo ""

# === Category 9: Security ===
echo -e "${BLUE}[9/11] Security${NC}"

if [ -f "SECURITY.md" ]; then
    check_pass "SECURITY.md present"
else
    check_fail "SECURITY.md missing"
fi

if grep -q "vulnerability" SECURITY.md 2>/dev/null; then
    check_pass "Vulnerability reporting process documented"
else
    check_warn "Vulnerability reporting not clear"
fi

if [ -f ".well-known/security.txt" ]; then
    check_pass "RFC 9116 security.txt present"
else
    check_fail "RFC 9116 security.txt missing"
fi

# Check for GPG signing support
if grep -q "GPG" README.md 2>/dev/null; then
    check_pass "Cryptographic signing documented (GPG)"
else
    check_warn "Cryptographic signing not documented"
fi

echo ""

# === Category 10: Governance ===
echo -e "${BLUE}[10/11] Community Governance${NC}"

if [ -f "CODE_OF_CONDUCT.md" ]; then
    check_pass "CODE_OF_CONDUCT.md present"
else
    check_fail "CODE_OF_CONDUCT.md missing"
fi

if grep -q "Contributor Covenant" CODE_OF_CONDUCT.md 2>/dev/null; then
    check_pass "Based on Contributor Covenant"
else
    check_warn "Code of Conduct format not standard"
fi

if [ -f "CONTRIBUTING.md" ]; then
    check_pass "CONTRIBUTING.md present"
else
    check_fail "CONTRIBUTING.md missing"
fi

echo ""

# === Category 11: Multi-Language Verification (Advanced) ===
echo -e "${BLUE}[11/11] Multi-Language Verification (Optional)${NC}"

# Count languages
LANG_COUNT=1  # Ada only currently

if [ $LANG_COUNT -eq 1 ]; then
    check_pass "Single-language project (Ada only) - verification straightforward"
else
    check_pass "Multi-language verification ($LANG_COUNT languages)"
fi

# Check for SPARK formal verification
if grep -r "pragma SPARK_Mode" src/ 2>/dev/null; then
    check_pass "SPARK formal verification in use"
else
    check_warn "SPARK formal verification not detected (optional, Platinum tier)"
fi

echo ""

# === Summary ===
TOTAL=$((PASS + FAIL + WARN))
PERCENTAGE=$(( (PASS * 100) / TOTAL ))

echo "═══════════════════════════════════════════════"
echo "  RSR Compliance Summary"
echo "═══════════════════════════════════════════════"
echo -e "${GREEN}✓ Passed:${NC} $PASS"
echo -e "${YELLOW}⚠ Warnings:${NC} $WARN"
echo -e "${RED}✗ Failed:${NC} $FAIL"
echo "Total Checks: $TOTAL"
echo ""
echo -e "Compliance Score: ${BLUE}${PERCENTAGE}%${NC}"
echo ""

# Determine tier
if [ $PERCENTAGE -ge 98 ]; then
    TIER="Platinum"
    COLOR=$BLUE
elif [ $PERCENTAGE -ge 92 ]; then
    TIER="Gold"
    COLOR=$YELLOW
elif [ $PERCENTAGE -ge 85 ]; then
    TIER="Silver"
    COLOR=$GREEN
elif [ $PERCENTAGE -ge 70 ]; then
    TIER="Bronze"
    COLOR=$YELLOW
else
    TIER="Non-Compliant"
    COLOR=$RED
fi

echo -e "RSR Tier: ${COLOR}${TIER}${NC}"
echo ""

# Recommendations
if [ $FAIL -gt 0 ]; then
    echo "Recommendations to improve compliance:"
    echo "  - Review failed checks above"
    echo "  - See RSR_COMPLIANCE.md for detailed guidance"
    echo "  - Run 'just rsr-check' after fixes"
fi

# Exit code
if [ $FAIL -gt 0 ]; then
    exit 1
else
    exit 0
fi
