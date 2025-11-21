# CLAUDE.md

## Project Overview

**Claim Forge** is an IP registration and timestamping system written in Ada that creates legally defensible intellectual property claims. It integrates Git, GPG signing, and blockchain timestamping (OpenTimestamps/Bitcoin) to establish provenance and ownership of creative works.

### Key Features

- **Type-safe implementation** in Ada/GNAT for reliability in legal contexts
- **Blockchain anchoring** via OpenTimestamps to Bitcoin for tamper-proof timestamps
- **Cryptographic signing** using GPG for commits and claims
- **Version control integration** with Git (signed commits/tags)
- **Multi-platform publishing** to GitLab/GitHub
- **Optional Wikidata registration** for public IP records

### Current Status

⚠️ **Code Recovery Mode**: The original codebase was lost. Only the deployment guide exists. This repository is being used to reconstruct the system (~75% reconstructable from deployment artifacts).

## Development Setup

### Prerequisites

- **GNAT** 2021+ (Ada compiler)
- **Git** 2.30+
- **GnuPG** 2.0+
- **Python** 3.8+
- **opentimestamps-client** (Python package)
- **Podman/Docker** (for containerized deployment)
- **SaltStack** (optional, for system automation)

### Installation

```bash
# Clone the repository
git clone https://github.com/Hyperpolymath/claim-forge.git
cd claim-forge

# Install Ada dependencies (using Alire if available)
alr get

# Or build directly with GNAT
make

# Install OpenTimestamps client
pip install opentimestamps-client

# Set up GPG key (if not already configured)
gpg --full-generate-key
```

### Running the Project

```bash
# Build the Ada application
make

# Run Claim Forge (interactive mode)
./bin/claim-forge

# Show help
./bin/claim-forge --help

# Containerized deployment
podman build -t claim-forge .
podman run -it claim-forge
```

## Project Structure

```
claim-forge/
├── bin/                  # Compiled binaries and scripts
│   ├── claim-forge      # Main executable
│   └── monitor.sh       # Monitoring script
├── src/                 # Ada source code
│   └── *.adb, *.ads     # Ada implementation and specification files
├── Makefile             # Build configuration
├── claim-forge.gpr      # GNAT project file
├── Containerfile        # Container build definition
├── .gitlab-ci.yml       # CI/CD pipeline
├── salt/                # SaltStack automation
│   └── claim-forge.sls
├── setup.sh             # Initial setup script
└── docs/                # Documentation
    └── DEPLOYMENT.md    # Deployment guide (primary artifact)
```

### Expected Ada Project Layout

The Ada implementation follows standard GNAT conventions:
- `.ads` files: Package specifications (interfaces)
- `.adb` files: Package bodies (implementations)
- `.gpr` file: GNAT project configuration

## Working with Claude Code

### Common Tasks

When working with Claude Code on this project, here are some common workflows:

#### Building the Project
```
Build the Ada application and fix any compilation errors
```

#### Reconstructing Lost Code
```
Implement [component] based on the deployment guide specification
```

#### Adding Features
```
Add a new feature for [description]. Make sure to:
- Follow Ada 2012 best practices
- Maintain type safety
- Update the GNAT project file if needed
- Test with actual GPG signing and OpenTimestamps
- Follow existing code patterns
```

#### Testing Claim Generation
```
Test the claim generation workflow end-to-end:
1. Create a test claim file
2. Generate GPG signature
3. Create OpenTimestamps proof
4. Verify timestamp on blockchain
```

#### Debugging
```
Investigate why [feature] is not working as expected. Check:
- GNAT compilation output
- GPG configuration and key availability
- OpenTimestamps client connectivity
- Git repository state
```

### Best Practices

1. **Type safety first** - leverage Ada's strong type system for correctness
2. **Test with real crypto** - verify GPG signing and OpenTimestamps integration
3. **Follow Ada conventions** - use standard package naming and organization
4. **Sign all commits** - use `git commit -S` for GPG signing
5. **Write clear commit messages** - use conventional commits format
6. **Update documentation** - keep docs in sync with code changes
7. **Ask before major refactors** - discuss significant architectural changes

### Useful Commands

```bash
# Build with GNAT
make clean && make

# Check Ada syntax
gnatcheck src/*.adb

# Format Ada code (if gnatpp is available)
gnatpp -rnb src/*.adb

# Test OpenTimestamps
ots stamp file.txt
ots verify file.txt.ots

# Verify GPG signature
gpg --verify file.sig file

# Build container
podman build -t claim-forge .

# Run in container
podman run -it --rm -v $(pwd):/workspace claim-forge
```

## Git Workflow

### Branch Naming

- Feature branches: `feature/description`
- Bug fixes: `fix/description`
- Claude branches: `claude/description-sessionId`

### Commit Guidelines

Follow conventional commits:
- `feat:` new features
- `fix:` bug fixes
- `docs:` documentation changes
- `test:` test additions or changes
- `refactor:` code refactoring
- `chore:` maintenance tasks

### Pull Request Process

1. Create a feature branch from main
2. Make your changes
3. Run tests and linting
4. Commit with clear messages
5. Push to your branch
6. Create a pull request with description

## Code Style

- **Indentation**: 3 spaces (Ada convention)
- **Naming**: Use Ada_Case for identifiers
- **Comments**: Use `--` for single-line comments
- **Packages**: One package per file pair (.ads/.adb)
- **Type safety**: Use strong typing, avoid type conversions
- **Error handling**: Use exceptions for exceptional conditions
- **Self-documenting code**: Clear variable and procedure names

## Testing

- Test with actual GPG keys and OpenTimestamps
- Verify blockchain anchoring end-to-end
- Test multi-platform push (GitLab + GitHub)
- Test edge cases: missing GPG key, network failures, invalid repo names
- Verify claim file format and content
- Test container deployment

## Reconstruction Status

### 🟢 Fully Reconstructable (90-100%)

These components can be recreated from the deployment guide:
- `Makefile` - Complete build configuration available
- `.gitlab-ci.yml` - Full CI/CD pipeline specification
- `Containerfile` - Complete container build definition
- `salt/claim-forge.sls` - SaltStack automation state
- `setup.sh` - Initial setup script logic
- Directory structure - Fully documented

### 🟡 Partially Reconstructable (40-70%)

These components have behavioral specs but missing implementation details:
- **Ada application core** - CLI behavior and workflow clear, but:
  - Exact argument parsing implementation unknown
  - Internal data structures and algorithms not specified
  - Error handling patterns not documented
- **Claim file format** - Structure/template not in deployment guide
- **Monitor script** (`bin/monitor.sh`) - Mentioned but not detailed

### 🔴 Unknown/Missing

These items require clarification:
- **Palimpsest License** - Custom license text not in guide
- **Wikidata automation** - Guide shows manual process only
- **Original use case specifics** - Academic/software/research context?
- **Testing artifacts** - No test suite documented

### Known Functional Requirements

From the deployment guide, the application:
1. Uses interactive prompts for: repo name, filename, description, license
2. Has `--help` flag
3. Generates claim files
4. Creates `.ots` timestamps
5. Performs GPG signing
6. Pushes to GitLab and GitHub
7. Creates signed Git tags

## Documentation

- Update README.md for user-facing changes
- Document public APIs
- Add inline comments for complex logic
- Keep this CLAUDE.md updated with new workflows

## Key Questions for Reconstruction

Before implementing, clarify with the user:

1. **Claim file format** - What structure/template should claim files use?
2. **Palimpsest License** - Where is the custom license text?
3. **Use case** - Academic papers? Software projects? Research data?
4. **Wikidata integration** - Should this be automated or manual?
5. **Other artifacts** - Any backups, binaries, or old emails available?
6. **Reconstruction scope** - Full rebuild or design review only?

## Next Steps for Development

### Phase 1: Infrastructure (Week 1)
- [ ] Create proper repository structure
- [ ] Implement Makefile
- [ ] Set up GNAT project file
- [ ] Create Containerfile
- [ ] Configure GitLab CI/CD

### Phase 2: Core Application (Week 2-3)
- [ ] Implement CLI argument parsing
- [ ] Create interactive prompt system
- [ ] Implement claim file generation
- [ ] Integrate GPG signing
- [ ] Integrate OpenTimestamps

### Phase 3: Integration (Week 4)
- [ ] Git operations (commit, tag, push)
- [ ] Multi-platform publishing
- [ ] Monitor script implementation
- [ ] End-to-end testing

## Troubleshooting

### Common Issues

**Issue**: GNAT compiler not found
**Solution**: Install GNAT via system package manager or download from AdaCore

**Issue**: GPG signing fails
**Solution**: Ensure GPG key is configured: `gpg --list-secret-keys`

**Issue**: OpenTimestamps connectivity problems
**Solution**: Check network, verify `ots` client installed: `pip install opentimestamps-client`

**Issue**: Container build fails
**Solution**: Verify Podman/Docker installed and running

## Resources

- [GNAT Documentation](https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn.html)
- [OpenTimestamps](https://opentimestamps.org/)
- [GPG Documentation](https://gnupg.org/documentation/)
- [Ada Programming Wikibook](https://en.wikibooks.org/wiki/Ada_Programming)
- [Project Deployment Guide](./docs/DEPLOYMENT.md) - Primary reference artifact

## Notes for Claude

### When Reconstructing Code
- **Reference the deployment guide** as the authoritative behavioral spec
- **Ask before implementing** ambiguous functionality
- **Use Ada 2012** features and best practices
- **Prioritize type safety** - it's a legal/IP tool, correctness is critical
- **Test cryptographic operations** thoroughly
- **Document assumptions** when spec is unclear

### When Working with Existing Code
- Always check for existing patterns before implementing new features
- Prefer editing existing files over creating new ones
- Run `make` after changes to verify compilation
- Test with actual GPG keys and OpenTimestamps
- Keep commits atomic and well-described
- Sign commits with GPG (`git commit -S`)

---

*Last updated: 2025-11-21*
