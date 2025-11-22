# Claim Forge - Justfile
# Modern task runner (alternative to Make)
# Install: https://github.com/casey/just
# Usage: just <recipe>

# Default recipe (runs when you type just "just")
default:
    @just --list

# Show all available recipes with descriptions
help:
    @just --list

# === Building ===

# Build in release mode (optimized)
build:
    @echo "Building Claim Forge (release mode)..."
    gprbuild -p -j0 -XBUILD_MODE=release -P claim-forge.gpr
    @echo "✓ Build complete: bin/claim-forge"

# Build in debug mode (with symbols)
build-debug:
    @echo "Building Claim Forge (debug mode)..."
    gprbuild -p -j0 -XBUILD_MODE=debug -P claim-forge.gpr
    @echo "✓ Debug build complete: bin/claim-forge"

# Clean build artifacts
clean:
    @echo "Cleaning build artifacts..."
    gprclean -P claim-forge.gpr
    rm -rf obj/
    @echo "✓ Clean complete"

# Clean everything including binaries
distclean: clean
    @echo "Removing binaries..."
    rm -rf bin/claim-forge
    @echo "✓ Distribution clean complete"

# === Testing ===

# Run all tests
test: build
    @echo "Running integration tests..."
    ./docs/examples/test-workflow.sh

# Run specific test
test-workflow:
    @echo "Running workflow test..."
    ./docs/examples/test-workflow.sh

# Verify compilation only (no tests)
verify:
    @echo "Verifying compilation..."
    gprbuild -p -j0 -XBUILD_MODE=release -P claim-forge.gpr -c

# === Code Quality ===

# Check Ada syntax
check:
    @echo "Checking Ada syntax..."
    -gnatcheck -P claim-forge.gpr

# Format Ada code
format:
    @echo "Formatting Ada code..."
    @echo "Finding .adb and .ads files..."
    find src -name "*.adb" -o -name "*.ads" | xargs -I {} sh -c 'echo "Formatting {}"; gnatpp -rnb {} || true'

# Count lines of code
loc:
    @echo "Lines of Code:"
    @echo "Ada source:"
    @find src -name "*.ad?" | xargs wc -l | tail -1
    @echo "\nScripts:"
    @find . -name "*.sh" | xargs wc -l | tail -1
    @echo "\nDocumentation:"
    @find . -name "*.md" | xargs wc -l | tail -1

# === RSR Compliance ===

# Check RSR compliance
rsr-check:
    @echo "Checking RSR compliance..."
    ./bin/rsr-verify.sh

# Show RSR compliance report
rsr-report:
    @cat RSR_COMPLIANCE.md

# Validate all RSR requirements
validate: build test rsr-check
    @echo "✓ All validations passed"

# === Installation ===

# Install to /usr/local (requires sudo)
install:
    @echo "Installing to /usr/local..."
    sudo make install PREFIX=/usr/local
    @echo "✓ Installed: /usr/local/bin/claim-forge"

# Install to custom prefix
install-to PREFIX:
    @echo "Installing to {{PREFIX}}..."
    sudo make install PREFIX={{PREFIX}}
    @echo "✓ Installed: {{PREFIX}}/bin/claim-forge"

# Uninstall from /usr/local
uninstall:
    @echo "Uninstalling from /usr/local..."
    sudo make uninstall PREFIX=/usr/local
    @echo "✓ Uninstalled"

# === Running ===

# Run Claim Forge with help
run-help:
    ./bin/claim-forge --help

# Run Claim Forge in interactive mode
run: build
    ./bin/claim-forge

# Run with example arguments (test mode)
run-example: build
    @echo "Running example claim registration..."
    ./bin/claim-forge \
        --repo example-project \
        --file README.md \
        --description "Example IP claim for testing" \
        --license "Palimpsest" \
        --no-push \
        --no-timestamp

# === Monitoring ===

# Run health check
health:
    ./bin/monitor.sh health

# Start continuous monitoring
monitor:
    ./bin/monitor.sh monitor

# Verify recent timestamps
verify-timestamps:
    ./bin/monitor.sh verify

# === Container ===

# Build container image
container-build:
    @echo "Building container image..."
    podman build -t claim-forge:latest .
    @echo "✓ Container built: claim-forge:latest"

# Run in container (interactive)
container-run:
    @echo "Running Claim Forge in container..."
    podman run -it --rm \
        -v $(pwd):/workspace:z \
        claim-forge:latest

# Shell in container
container-shell:
    @echo "Opening shell in container..."
    podman run -it --rm \
        -v $(pwd):/workspace:z \
        claim-forge:latest \
        /bin/bash

# === Development ===

# Watch for changes and rebuild
watch:
    @echo "Watching for changes (requires inotify-tools)..."
    while true; do \
        inotifywait -e modify -r src/; \
        just build; \
    done

# Quick dev cycle: clean, build, test
dev: clean build test
    @echo "✓ Development cycle complete"

# Full rebuild from scratch
rebuild: distclean build
    @echo "✓ Full rebuild complete"

# === Documentation ===

# Generate documentation (placeholder for future)
docs:
    @echo "Documentation generation not yet implemented"
    @echo "See: README.md, ASSUMPTIONS.md, docs/"

# Serve documentation locally (placeholder)
docs-serve:
    @echo "Starting local docs server..."
    @echo "Opening docs/examples/quick-start.md"
    @python3 -m http.server 8000 -d .

# === Git ===

# Show git status with helpful info
status:
    @echo "Git Status:"
    @git status
    @echo ""
    @echo "Recent commits:"
    @git log --oneline -5
    @echo ""
    @echo "Current branch:"
    @git branch --show-current

# Create a signed commit with conventional commit format
commit MESSAGE:
    @echo "Creating signed commit: {{MESSAGE}}"
    git add -A
    git commit -S -m "{{MESSAGE}}"

# Tag current commit
tag VERSION:
    @echo "Creating signed tag: v{{VERSION}}"
    git tag -s -m "Release v{{VERSION}}" v{{VERSION}}

# Push to origin with tags
push:
    @echo "Pushing to origin..."
    git push origin $(git branch --show-current)
    git push --tags
    @echo "✓ Pushed successfully"

# === Release ===

# Prepare a new release
release VERSION: clean build test
    @echo "Preparing release v{{VERSION}}..."
    @echo "1. Updating VERSION file..."
    @echo "{{VERSION}}" > VERSION
    @echo "2. Updating CHANGELOG.md..."
    @echo "Please manually update CHANGELOG.md"
    @echo "3. Ready to commit and tag"
    @echo ""
    @echo "Next steps:"
    @echo "  1. Review CHANGELOG.md"
    @echo "  2. just commit 'chore: release v{{VERSION}}'"
    @echo "  3. just tag {{VERSION}}"
    @echo "  4. just push"

# === Misc ===

# Show project statistics
stats:
    @echo "Claim Forge Statistics"
    @echo "======================"
    @echo "Files:"
    @find src -name "*.ad?" | wc -l | xargs echo "  Ada source files:"
    @find docs -name "*.md" | wc -l | xargs echo "  Markdown docs:"
    @find . -name "*.sh" | wc -l | xargs echo "  Shell scripts:"
    @echo ""
    @echo "Lines of Code:"
    @just loc
    @echo ""
    @echo "Git:"
    @git rev-list --count HEAD | xargs echo "  Commits:"
    @git tag | wc -l | xargs echo "  Tags:"
    @echo ""
    @echo "Contributors:"
    @git log --format='%aN' | sort -u | wc -l | xargs echo "  Unique authors:"

# Check dependencies
deps:
    @echo "Checking dependencies..."
    @echo "GNAT:"
    @gnat --version | head -1 || echo "  ✗ Not found"
    @echo "Git:"
    @git --version || echo "  ✗ Not found"
    @echo "GPG:"
    @gpg --version | head -1 || echo "  ✗ Not found"
    @echo "OpenTimestamps:"
    @ots --version 2>/dev/null || echo "  ✗ Not found (run: pip3 install opentimestamps-client)"
    @echo "Podman:"
    @podman --version || echo "  ✗ Not found (optional)"
    @echo "Just:"
    @just --version || echo "  ✗ Not found"

# Run setup script
setup:
    @echo "Running setup script..."
    ./setup.sh

# === Aliases ===

# Alias for build
b: build

# Alias for test
t: test

# Alias for run
r: run

# Alias for clean
c: clean

# Alias for help
h: help
