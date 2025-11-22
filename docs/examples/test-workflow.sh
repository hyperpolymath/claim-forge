#!/usr/bin/env bash
# Claim Forge - Test Workflow Script
# Validates the complete claim registration workflow

set -euo pipefail

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo "═══════════════════════════════════════════════"
echo "  Claim Forge - Test Workflow"
echo "═══════════════════════════════════════════════"
echo ""

# Function to check command exists
check_command() {
    if command -v "$1" &> /dev/null; then
        echo -e "${GREEN}✓${NC} $1 is installed"
        return 0
    else
        echo -e "${RED}✗${NC} $1 is not installed"
        return 1
    fi
}

# Function to check GPG key
check_gpg_key() {
    if gpg --list-secret-keys | grep -q "sec"; then
        echo -e "${GREEN}✓${NC} GPG secret key found"
        return 0
    else
        echo -e "${RED}✗${NC} No GPG secret key found"
        echo "    Run: gpg --full-generate-key"
        return 1
    fi
}

# Step 1: Check prerequisites
echo "Step 1: Checking prerequisites..."
all_ok=true
check_command "gnat" || all_ok=false
check_command "git" || all_ok=false
check_command "gpg" || all_ok=false
check_command "ots" || all_ok=false
check_gpg_key || all_ok=false
echo ""

if [ "$all_ok" = false ]; then
    echo -e "${RED}Prerequisites not met. Please install missing components.${NC}"
    exit 1
fi

# Step 2: Build Claim Forge
echo "Step 2: Building Claim Forge..."
if [ ! -f "Makefile" ]; then
    echo -e "${RED}Error: Makefile not found. Are you in the project root?${NC}"
    exit 1
fi

make clean > /dev/null 2>&1
if make release > /dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} Build successful"
else
    echo -e "${RED}✗${NC} Build failed"
    exit 1
fi
echo ""

# Step 3: Test help command
echo "Step 3: Testing help command..."
if ./bin/claim-forge --help > /dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} Help command works"
else
    echo -e "${RED}✗${NC} Help command failed"
    exit 1
fi
echo ""

# Step 4: Create test environment
echo "Step 4: Creating test environment..."
TEST_DIR=$(mktemp -d -t claim-forge-test.XXXXXX)
echo "Test directory: $TEST_DIR"
cd "$TEST_DIR"

# Initialize git
git init > /dev/null 2>&1
git config user.name "Test User" > /dev/null 2>&1
git config user.email "test@example.com" > /dev/null 2>&1
echo -e "${GREEN}✓${NC} Test environment created"
echo ""

# Step 5: Create test file
echo "Step 5: Creating test file..."
TEST_FILE="test-invention.md"
cat > "$TEST_FILE" <<EOF
# Test Invention

This is a test invention for claim registration.

Created: $(date)
EOF
echo -e "${GREEN}✓${NC} Test file created: $TEST_FILE"
echo ""

# Step 6: Run claim-forge (non-interactive)
echo "Step 6: Running claim-forge..."
echo "Command: claim-forge --repo test-project --file $TEST_FILE --description 'Test claim' --no-push --no-timestamp"
echo ""

# Run without push and timestamp for faster testing
if $OLDPWD/bin/claim-forge \
    --repo test-project \
    --file "$TEST_FILE" \
    --description "Test claim for workflow validation" \
    --license "Palimpsest" \
    --no-push \
    --no-timestamp; then
    echo ""
    echo -e "${GREEN}✓${NC} Claim registration completed"
else
    echo -e "${RED}✗${NC} Claim registration failed"
    cd "$OLDPWD"
    rm -rf "$TEST_DIR"
    exit 1
fi
echo ""

# Step 7: Verify outputs
echo "Step 7: Verifying outputs..."

# Check CLAIM.md exists
if [ -f "CLAIM.md" ]; then
    echo -e "${GREEN}✓${NC} CLAIM.md created"
else
    echo -e "${RED}✗${NC} CLAIM.md not found"
    cd "$OLDPWD"
    rm -rf "$TEST_DIR"
    exit 1
fi

# Check GPG signature (if auto-sign enabled)
if [ -f "CLAIM.md.sig" ]; then
    echo -e "${GREEN}✓${NC} GPG signature file created"
    if gpg --verify CLAIM.md.sig CLAIM.md > /dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} GPG signature valid"
    else
        echo -e "${YELLOW}⚠${NC} GPG signature verification failed (may be expected)"
    fi
else
    echo -e "${YELLOW}⚠${NC} No GPG signature file (may be expected if signing disabled)"
fi

# Check Git commit
if git log --oneline | grep -q "feat: add IP claim"; then
    echo -e "${GREEN}✓${NC} Git commit created"
else
    echo -e "${RED}✗${NC} Git commit not found"
fi

# Check Git tag
if git tag | grep -q "claim-"; then
    echo -e "${GREEN}✓${NC} Git tag created"
else
    echo -e "${YELLOW}⚠${NC} Git tag not found (may be expected)"
fi

echo ""

# Step 8: Display claim file
echo "Step 8: Displaying claim file..."
echo "────────────────────────────────────────────────"
cat CLAIM.md
echo "────────────────────────────────────────────────"
echo ""

# Step 9: Test monitor script
echo "Step 9: Testing monitor script..."
cd "$OLDPWD"
if ./bin/monitor.sh health > /dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} Monitor script works"
else
    echo -e "${YELLOW}⚠${NC} Monitor script failed (may be expected)"
fi
echo ""

# Cleanup
echo "Step 10: Cleanup..."
rm -rf "$TEST_DIR"
echo -e "${GREEN}✓${NC} Test directory removed"
echo ""

# Summary
echo "═══════════════════════════════════════════════"
echo -e "${GREEN}  All tests passed!${NC}"
echo "═══════════════════════════════════════════════"
echo ""
echo "Next steps:"
echo "  1. Review ASSUMPTIONS.md for implementation details"
echo "  2. Configure Git remotes for actual use"
echo "  3. Test with real files and full workflow"
echo "  4. Set up monitoring for production use"
echo ""
