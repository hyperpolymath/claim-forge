# Claim Forge

**IP Registration & Blockchain Timestamping System**

Claim Forge is a type-safe Ada application that creates legally defensible intellectual property claims through cryptographic signing (GPG), blockchain timestamping (OpenTimestamps/Bitcoin), and version control integration (Git).

## Features

- 🔒 **Cryptographic Signing**: GPG signatures for authenticity
- ⛓️ **Blockchain Anchoring**: OpenTimestamps integration for tamper-proof timestamps
- 📝 **Automated Claim Generation**: Structured claim files with metadata
- 🔐 **Type-Safe Implementation**: Ada 2012 for reliability in legal contexts
- 🚀 **Multi-Platform Publishing**: Automatic push to GitLab and GitHub
- 📊 **Version Control**: Signed Git commits and tags for provenance
- 🎯 **CLI & Interactive Modes**: Flexible usage options

## Quick Start

### Prerequisites

- GNAT compiler (2021+)
- Git (2.30+)
- GnuPG (2.0+)
- Python 3.8+ with `opentimestamps-client`
- GPG key configured for signing

### Installation

```bash
# Clone the repository
git clone https://github.com/Hyperpolymath/claim-forge.git
cd claim-forge

# Run setup script (recommended)
./setup.sh

# Or build manually
make clean && make release
```

### Usage

#### Interactive Mode (Default)

```bash
./bin/claim-forge
```

This will prompt you for:
- Repository name
- File to claim
- Description
- License type

#### Command-Line Mode

```bash
./bin/claim-forge \
  --repo my-research \
  --file paper.pdf \
  --description "Novel algorithm for distributed consensus" \
  --license "Palimpsest"
```

#### Display Help

```bash
./bin/claim-forge --help
```

## How It Works

Claim Forge executes a 7-step workflow:

1. **Generate Claim File** - Creates a structured CLAIM.md with metadata
2. **Configure Git** - Initializes or configures the Git repository
3. **GPG Signing** - Signs the claim file with your GPG key
4. **OpenTimestamps** - Creates blockchain-anchored timestamp proof
5. **Git Commit** - Commits claim with signed commit
6. **Create Tag** - Creates a signed Git tag
7. **Publish** - Pushes to GitLab and/or GitHub

### Verification

All claims can be independently verified:

```bash
# Verify GPG signature
gpg --verify CLAIM.md.sig CLAIM.md

# Verify blockchain timestamp
ots verify CLAIM.md.ots

# Verify Git signature
git verify-commit HEAD
git verify-tag claim-my-project
```

## Configuration

Create a TOML configuration file (optional):

```toml
[claim-forge]
default_license = "Palimpsest"
gitlab_enabled = true
github_enabled = true
ots_enabled = true
auto_sign = true
```

Use with `--config`:

```bash
./bin/claim-forge --config claim-forge.toml --interactive
```

## Command-Line Options

```
Options:
  -h, --help              Show help message
  -v, --version           Show version information
  -i, --interactive       Run in interactive mode (default)
  -c, --config FILE       Use custom configuration file
  --repo NAME             Repository name
  --file PATH             File to claim
  --description TEXT      Claim description
  --license TYPE          License type (default: Palimpsest)
  --no-timestamp          Skip OpenTimestamps
  --no-push               Skip git push
```

## Building from Source

### Standard Build

```bash
make                 # Build in release mode
make debug           # Build with debug symbols
make clean           # Clean build artifacts
```

### Using GPRbuild directly

```bash
gprbuild -P claim-forge.gpr -XBUILD_MODE=release
```

### Container Build

```bash
# Build container
podman build -t claim-forge .

# Run in container
podman run -it --rm -v $(pwd):/workspace claim-forge

# Interactive shell
podman run -it --rm -v $(pwd):/workspace claim-forge /bin/bash
```

## Monitoring

Use the monitoring script to check system health:

```bash
# Single health check
./bin/monitor.sh health

# Continuous monitoring
./bin/monitor.sh monitor

# Check specific component
./bin/monitor.sh check gpg
./bin/monitor.sh check ots

# Verify recent timestamps
./bin/monitor.sh verify
```

## Development

### Project Structure

```
claim-forge/
├── bin/                  # Compiled binaries and scripts
│   ├── claim-forge      # Main executable
│   └── monitor.sh       # Monitoring script
├── src/                 # Ada source code
│   ├── claim_forge.adb  # Main entry point
│   ├── cli.*            # Command-line parsing
│   ├── config.*         # Configuration management
│   ├── interactive.*    # Interactive prompts
│   ├── workflow.*       # Workflow orchestration
│   ├── claim.*          # Claim file generation
│   ├── gpg.*            # GPG integration
│   ├── opentimestamps.* # OpenTimestamps integration
│   ├── git_ops.*        # Git operations
│   └── publisher.*      # Multi-platform publishing
├── Makefile             # Build configuration
├── claim-forge.gpr      # GNAT project file
├── Containerfile        # Container build definition
└── .gitlab-ci.yml       # CI/CD pipeline
```

### Code Style

- **Indentation**: 3 spaces (Ada convention)
- **Naming**: Ada_Case for identifiers
- **Type Safety**: Strong typing, avoid conversions
- **Comments**: Use `--` for documentation

### Building Documentation

```bash
# Format code
make format

# Check syntax
make check
```

## Deployment

### System-Wide Installation

```bash
sudo make install PREFIX=/usr/local
```

### Using SaltStack

```bash
salt '*' state.apply claim-forge
```

### CI/CD

GitLab CI/CD pipeline includes:
- Automated builds
- Syntax checking
- Container image creation
- Release artifact generation

## Troubleshooting

### GPG Signing Fails

Ensure you have a GPG key configured:

```bash
gpg --list-secret-keys
git config --global user.signingkey YOUR_KEY_ID
git config --global commit.gpgsign true
```

### OpenTimestamps Not Working

Install the client:

```bash
pip3 install opentimestamps-client
```

### Git Push Fails

Configure Git remotes manually:

```bash
git remote add gitlab git@gitlab.com:username/repo.git
git remote add github git@github.com:username/repo.git
```

## Legal & License

Claim Forge is designed to create legally defensible intellectual property claims. The generated claims include:

- Cryptographic signatures (GPG)
- Blockchain timestamps (Bitcoin via OpenTimestamps)
- Version control provenance (Git)
- Structured metadata

This triple-layered verification provides strong evidence of:
- **Existence**: The work existed at a specific time
- **Ownership**: The work was created/owned by the key holder
- **Integrity**: The work has not been modified since claiming

## Contributing

This project was reconstructed from a deployment guide. See [ASSUMPTIONS.md](./ASSUMPTIONS.md) for details on implementation decisions.

Contributions welcome! Please:
- Follow Ada 2012 best practices
- Maintain type safety
- Test with actual GPG and OpenTimestamps
- Sign your commits with GPG

## Resources

- [GNAT Documentation](https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn.html)
- [OpenTimestamps](https://opentimestamps.org/)
- [GPG Documentation](https://gnupg.org/documentation/)
- [Project Guide](./CLAUDE.md) - For Claude Code usage

## Support

- Issues: https://github.com/Hyperpolymath/claim-forge/issues
- Documentation: See [CLAUDE.md](./CLAUDE.md)

## Version

**v1.0.0** - Reconstructed Implementation

---

*Built with Ada for type safety and reliability in legal contexts.*
