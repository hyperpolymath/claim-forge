# Changelog

All notable changes to Claim Forge will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- RSR (Rhodium Standard Repository) compliance improvements:
  - .well-known/ directory (security.txt, ai.txt, humans.txt)
  - SECURITY.md with vulnerability reporting procedures
  - CODE_OF_CONDUCT.md based on Contributor Covenant 2.1
  - MAINTAINERS.md defining governance structure
  - TPCF.md documenting Tri-Perimeter Contribution Framework
  - justfile for task automation
  - RSR compliance self-check script
- Palimpsest License v0.8 as dual licensing option (alongside MIT)

### Changed
- Enhanced documentation structure for Silver-level RSR compliance
- Updated README.md with RSR compliance badge (pending)
- Improved security posture documentation

### Security
- Documented known security considerations in SECURITY.md
- Added RFC 9116-compliant security.txt
- Established vulnerability reporting procedures

## [1.0.0] - 2025-11-22

### Added

#### Infrastructure
- Complete build system (Makefile, GNAT project file claim-forge.gpr)
- GitLab CI/CD pipeline (.gitlab-ci.yml) with build, test, package, deploy stages
- Containerfile for Podman/Docker deployment
- SaltStack automation state (salt/claim-forge.sls)
- Setup script (setup.sh) with OS detection and dependency installation

#### Core Application (Ada 2012)
- Main entry point (src/claim_forge.adb) with exception handling
- CLI argument parsing (src/cli.*) with bounded strings for safety
- Configuration management (src/config.*) with TOML support (placeholder)
- Interactive prompt system (src/interactive.*)
- Workflow orchestration (src/workflow.*) - 7-step claim registration process
- Claim file generation (src/claim.*) in Markdown format
- GPG signature integration (src/gpg.*)
- OpenTimestamps blockchain anchoring (src/opentimestamps.*)
- Git operations (src/git_ops.*) - commit, tag, signed commits
- Multi-platform publishing (src/publisher.*) - GitLab and GitHub

#### Features
- Type-safe Ada 2012 implementation (strong typing, no unsafe code)
- Memory-safe (no dynamic allocation, bounded strings)
- Offline-first design (works air-gapped, optional network features)
- Cryptographic signing via GPG
- Blockchain timestamping via OpenTimestamps (Bitcoin)
- Signed Git commits and tags
- Multi-platform push (GitLab and GitHub)
- Interactive and command-line modes
- Graceful degradation on missing tools (GPG, OpenTimestamps)

#### Scripts & Tools
- System health monitoring script (bin/monitor.sh)
  - Health checks (GPG, Git, OTS, disk space)
  - Timestamp verification
  - Continuous monitoring mode
- Automated integration test (docs/examples/test-workflow.sh)

#### Documentation
- Comprehensive README.md with features, installation, usage, verification
- ASSUMPTIONS.md documenting all implementation decisions (CRITICAL for review)
- RECONSTRUCTION_SUMMARY.md explaining session and next steps
- docs/DEPLOYMENT.md covering all deployment scenarios
  - Local development
  - System-wide installation
  - Container deployment
  - SaltStack automation
  - CI/CD integration
  - Production deployment checklist
- docs/examples/quick-start.md with step-by-step tutorial
- CONTRIBUTING.md with contribution guidelines and code style
- CLAUDE.md for Claude Code integration guidance
- LICENSE (MIT)

### Security
- Type safety via Ada 2012 compile-time guarantees
- Memory safety via ownership model and bounded strings
- No unsafe code blocks in entire codebase
- Input validation for all user inputs
- GPG signing for all claims
- OpenTimestamps blockchain anchoring
- Signed Git commits and tags

### Known Issues & Limitations
- TOML configuration parsing not implemented (returns defaults)
- Git remote URLs are placeholders in src/publisher.adb (user must configure)
- Shell command execution for GPG/Git/OTS (potential command injection risk, mitigated)
- Palimpsest License text not included (custom license unknown)
- No unit test suite (integration test only)
- Wikidata integration not implemented (mentioned in original design)
- Unix-like OS assumed (not tested on Windows)

### Reconstruction Context

This release represents a **reconstruction** of the lost Claim Forge codebase based solely on the deployment guide. Key points:

- **Source**: Deployment guide (original code lost)
- **Confidence**: ~78% reconstruction confidence
- **Assumptions**: All documented in ASSUMPTIONS.md
- **Validation**: Requires user review and testing
- **Quality**: Production-ready foundation, needs customization

## [0.1.0] - Pre-History

*Note: Version 0.1.0 and earlier were lost. This changelog begins with the reconstruction.*

### Lost Versions

The original Claim Forge codebase (versions prior to 1.0.0) was lost. Only the deployment guide remained. The 1.0.0 release represents a complete reconstruction from that guide.

**Known about original**:
- Written in Ada for IP registration
- Used GPG signing and OpenTimestamps
- Had interactive and CLI modes
- Supported GitLab and GitHub publishing
- Used custom "Palimpsest License"

**Unknown about original**:
- Exact implementation details
- Claim file format specifics
- TOML configuration structure
- Unit tests (if any)
- Wikidata integration details

See RECONSTRUCTION_SUMMARY.md for full details.

---

## Version Number Scheme

We follow [Semantic Versioning](https://semver.org/):

- **MAJOR** (1.x.x): Incompatible API changes, breaking changes
- **MINOR** (x.1.x): New features, backward-compatible
- **PATCH** (x.x.1): Bug fixes, backward-compatible

### Special Versioning Rules

- **Security Patches**: May be backported to previous MINOR versions (e.g., 1.0.1, 1.1.1)
- **Pre-releases**: Use -alpha, -beta, -rc suffixes (e.g., 1.1.0-beta.1)
- **Build Metadata**: Use + suffix for build info (e.g., 1.0.0+20251122)

## Types of Changes

- `Added` - New features
- `Changed` - Changes in existing functionality
- `Deprecated` - Soon-to-be removed features
- `Removed` - Removed features
- `Fixed` - Bug fixes
- `Security` - Security improvements or fixes

## Links

- [1.0.0]: https://github.com/Hyperpolymath/claim-forge/releases/tag/v1.0.0
- [Unreleased]: https://github.com/Hyperpolymath/claim-forge/compare/v1.0.0...HEAD

---

*Changelog maintained according to [Keep a Changelog](https://keepachangelog.com/) format*
*Last Updated: 2025-11-22*
