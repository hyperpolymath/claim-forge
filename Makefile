# Claim Forge - Makefile
# Ada-based IP registration and timestamping system

# Project configuration
PROJECT_NAME = claim-forge
GPR_FILE = $(PROJECT_NAME).gpr
BIN_DIR = bin
SRC_DIR = src
OBJ_DIR = obj

# Compiler configuration
GNAT = gnatmake
GPRBUILD = gprbuild
GPRCLEAN = gprclean

# Build flags
BUILD_MODE ?= release
GNATMAKE_FLAGS = -p -j0
GPRBUILD_FLAGS = -p -j0 -XBUILD_MODE=$(BUILD_MODE)

# Installation paths
PREFIX ?= /usr/local
BINDIR = $(PREFIX)/bin
DATADIR = $(PREFIX)/share/$(PROJECT_NAME)

.PHONY: all build clean install uninstall test help

# Default target
all: build

# Build the project
build:
	@echo "Building $(PROJECT_NAME) in $(BUILD_MODE) mode..."
	@mkdir -p $(BIN_DIR) $(OBJ_DIR)
	$(GPRBUILD) $(GPRBUILD_FLAGS) -P $(GPR_FILE)
	@echo "Build complete: $(BIN_DIR)/$(PROJECT_NAME)"

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	$(GPRCLEAN) -P $(GPR_FILE)
	@rm -rf $(OBJ_DIR)
	@echo "Clean complete"

# Clean everything including binaries
distclean: clean
	@rm -rf $(BIN_DIR)
	@echo "Distribution clean complete"

# Install the application
install: build
	@echo "Installing $(PROJECT_NAME) to $(PREFIX)..."
	@install -d $(BINDIR)
	@install -m 755 $(BIN_DIR)/$(PROJECT_NAME) $(BINDIR)/
	@install -d $(DATADIR)
	@echo "Installation complete"

# Uninstall the application
uninstall:
	@echo "Uninstalling $(PROJECT_NAME)..."
	@rm -f $(BINDIR)/$(PROJECT_NAME)
	@rm -rf $(DATADIR)
	@echo "Uninstall complete"

# Run tests
test: build
	@echo "Running tests..."
	@echo "Note: Test suite to be implemented"

# Development build (debug mode)
debug:
	@$(MAKE) BUILD_MODE=debug build

# Release build (optimized)
release:
	@$(MAKE) BUILD_MODE=release build

# Check code syntax
check:
	@echo "Checking Ada syntax..."
	@gnatcheck -P $(GPR_FILE) || true

# Format code
format:
	@echo "Formatting Ada code..."
	@find $(SRC_DIR) -name "*.adb" -o -name "*.ads" | xargs gnatpp -rnb || true

# Show help
help:
	@echo "Claim Forge - Build System"
	@echo ""
	@echo "Available targets:"
	@echo "  all       - Build the project (default)"
	@echo "  build     - Build the project"
	@echo "  clean     - Remove build artifacts"
	@echo "  distclean - Remove all generated files"
	@echo "  install   - Install to $(PREFIX)"
	@echo "  uninstall - Remove installed files"
	@echo "  test      - Run test suite"
	@echo "  debug     - Build in debug mode"
	@echo "  release   - Build in release mode"
	@echo "  check     - Check code syntax"
	@echo "  format    - Format source code"
	@echo "  help      - Show this help message"
	@echo ""
	@echo "Variables:"
	@echo "  PREFIX      - Installation prefix (default: /usr/local)"
	@echo "  BUILD_MODE  - Build mode: debug or release (default: release)"
