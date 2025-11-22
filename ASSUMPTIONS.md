# Claim Forge - Implementation Assumptions

This document records all assumptions made during the reconstruction of Claim Forge from the deployment guide. Since the original codebase was lost, these decisions represent reasonable interpretations of the behavioral specification.

**Last Updated**: 2025-11-22
**Reconstruction Status**: ~75% from deployment artifacts, 25% reasonable assumptions

---

## Overview

The original Claim Forge codebase was lost, leaving only the deployment guide as a reference. This implementation reconstructs the system based on:

1. **Known behavioral specifications** from the deployment guide
2. **Standard Ada/GNAT best practices** for similar applications
3. **Reasonable assumptions** where details were unclear
4. **Common patterns** for CLI tools and cryptographic workflows

---

## Architecture Assumptions

### 1. Modular Ada Package Design

**Assumption**: The original used a modular package structure with clear separation of concerns.

**Rationale**:
- Deployment guide mentioned multiple responsibilities (CLI, GPG, Git, OTS, etc.)
- Ada best practices favor package-based modularity
- Type safety requirements suggest interface specifications (.ads files)

**Implementation**:
```
claim_forge.adb     - Main entry point
cli.*               - Command-line argument parsing
config.*            - Configuration management
interactive.*       - User prompts
workflow.*          - Orchestration logic
claim.*             - Claim file generation
gpg.*               - GPG signing
opentimestamps.*    - OpenTimestamps integration
git_ops.*           - Git operations
publisher.*         - Multi-platform publishing
```

### 2. Exception-Based Error Handling

**Assumption**: Each module has its own exception type for domain-specific errors.

**Rationale**:
- Ada 2012 best practice for error handling
- Allows fine-grained error recovery at appropriate levels
- Clear separation of error domains (CLI vs GPG vs Git, etc.)

**Implementation**:
- `CLI.Invalid_Arguments`
- `Config.Configuration_Error`
- `Workflow.Workflow_Error`
- `GPG.GPG_Error`
- `OpenTimestamps.OTS_Error`
- `Git_Ops.Git_Error`
- `Publisher.Publisher_Error`

### 3. Bounded Strings for User Input

**Assumption**: Used fixed-size bounded strings to avoid dynamic allocation.

**Rationale**:
- Ada safety: no dynamic allocation reduces complexity
- CLI arguments typically under 1024 characters
- Predictable memory usage for legal/embedded contexts

**Implementation**:
```ada
Max_String_Length : constant := 1024;
subtype Bounded_String is String (1 .. Max_String_Length);
```

---

## Claim File Format Assumptions

### 4. Markdown-Based Claim Files

**Assumption**: Claim files use Markdown format named `CLAIM.md`.

**Rationale**:
- Deployment guide mentioned "claim files" but not specific format
- Markdown is human-readable and widely supported
- Standard in documentation contexts
- Easy to version control and diff

**Structure**:
```markdown
# Intellectual Property Claim

## Claim Information
- **Claimed Work**: [file path]
- **Description**: [description]
- **License**: [license type]
- **Claim Date**: [ISO 8601 timestamp]

## Declaration
[Boilerplate legal text]

## Timestamp Verification
[Instructions for OTS verification]

## Cryptographic Signature
[Instructions for GPG verification]

## License Terms
[License text]
```

### 5. Palimpsest License as Default

**Assumption**: "Palimpsest" is the default license, but no full license text available.

**Rationale**:
- Deployment guide mentioned "Palimpsest License"
- Appears to be custom license (not found in standard license databases)
- Left as placeholder string for now

**TODO**: User must provide actual Palimpsest License text if needed.

---

## Command-Line Interface Assumptions

### 6. Interactive Mode by Default

**Assumption**: Running without arguments defaults to interactive mode with prompts.

**Rationale**:
- User-friendly for occasional use
- Deployment guide showed interactive prompts
- Common pattern for CLI tools

### 7. Standard CLI Flags

**Assumption**: Used common Unix-style flags.

**Implementation**:
```
-h, --help          Help message
-v, --version       Version information
-i, --interactive   Interactive mode
-c, --config FILE   Config file
--repo NAME         Repository name
--file PATH         File to claim
--description TEXT  Description
--license TYPE      License type
--no-timestamp      Skip OpenTimestamps
--no-push           Skip remote push
```

---

## Integration Assumptions

### 8. Shell Command Execution for External Tools

**Assumption**: GPG, OpenTimestamps, and Git are called via shell commands using `GNAT.OS_Lib.Spawn`.

**Rationale**:
- Ada has limited native bindings for GPG/Git
- Shell execution is standard for Ada CLI tools
- Original deployment guide showed command-line usage patterns

**Risk**: Requires external tools in PATH. Not fully type-safe across process boundary.

**Alternative Considered**: Native bindings (rejected due to complexity and maintenance burden).

### 9. Graceful Degradation for Missing Features

**Assumption**: System attempts signed operations but falls back gracefully if GPG key unavailable.

**Rationale**:
- Better user experience than hard failures
- Deployment guide didn't specify behavior on missing GPG key
- Allows testing without full crypto setup

**Implementation**:
- Attempts `git commit -S` (signed), falls back to `git commit` if fails
- Attempts signed tags, falls back to unsigned if fails
- Logs warnings instead of hard errors

### 10. Git Remote Configuration

**Assumption**: User must manually configure Git remotes; system only attempts push.

**Rationale**:
- Deployment guide showed GitLab/GitHub push but didn't specify remote setup
- Security: shouldn't auto-configure SSH keys or credentials
- Users have different remote naming conventions

**Implementation**:
```ada
-- Attempts to add remote (fails silently if exists)
git remote add gitlab git@gitlab.com:user/repo.git
git remote add github git@github.com:user/repo.git

-- Push (warns if fails)
git push gitlab main --tags
git push github main --tags
```

**Note**: User must update remote URLs in Publisher module or configure manually.

---

## Configuration Assumptions

### 11. TOML Configuration Format

**Assumption**: Configuration files use TOML format (though parsing not fully implemented).

**Rationale**:
- Deployment guide mentioned configuration but didn't specify format
- TOML is Ada-friendly and widely used
- Simple key-value structure

**Current Status**: Placeholder - returns defaults. TOML parsing TODO.

**Default Configuration**:
```ada
Default_License    : "Palimpsest"
GitLab_Enabled     : True
GitHub_Enabled     : True
OTS_Enabled        : True
Auto_Sign          : True
```

---

## Git Workflow Assumptions

### 12. Conventional Commits Format

**Assumption**: Commit messages use conventional commits with `feat:` prefix.

**Rationale**:
- Modern best practice
- Matches CLAUDE.md guidelines
- Clear semantic versioning

**Format**:
```
feat: add IP claim for [file]

[description]
```

### 13. Tag Naming Convention

**Assumption**: Tags are prefixed with `claim-` followed by repo name.

**Rationale**:
- Clear distinction from version tags
- Searchable/filterable
- Matches claim-centric workflow

**Example**: `claim-my-research-project`

---

## OpenTimestamps Assumptions

### 14. Background Timestamp Upgrades

**Assumption**: Initial `ots stamp` creates pending timestamp, upgrades later automatically.

**Rationale**:
- OpenTimestamps works this way inherently
- Initial stamp is instant (calendar server)
- Blockchain anchoring happens in next Bitcoin block (~10 minutes)

**Implementation**: No explicit upgrade step; monitor script can verify later.

---

## Monitoring Assumptions

### 15. Health Check Script

**Assumption**: Separate monitoring script (`bin/monitor.sh`) for system health.

**Rationale**:
- Deployment guide mentioned monitoring
- Bash is simpler than Ada for system checks
- Allows independent health verification

**Features**:
- Check binary, GPG, OTS, Git
- Monitor disk space
- Verify recent timestamps
- Continuous monitoring mode

---

## Build System Assumptions

### 16. Makefile + GNAT Project File

**Assumption**: Standard GNAT build system with Make wrapper.

**Rationale**:
- Standard Ada project structure
- Deployment guide showed `make` commands
- GPR files are idiomatic for GNAT

**Build Modes**:
- `release`: Optimized, no debug symbols
- `debug`: Debug symbols, assertions enabled

### 17. Fedora-Based Container

**Assumption**: Container based on Fedora with `gcc-gnat` package.

**Rationale**:
- Deployment guide implied Fedora/RHEL environment
- `gcc-gnat` available in Fedora repos
- Python 3 and pip readily available

---

## Security Assumptions

### 18. User Manages GPG Keys

**Assumption**: System never creates or manages GPG keys; user must configure.

**Rationale**:
- Security best practice
- Key management is user responsibility
- Different trust models per deployment

### 19. No Credential Storage

**Assumption**: System never stores Git credentials, SSH keys, or GPG passphrases.

**Rationale**:
- Security principle: credentials managed by OS/user
- Git uses SSH agent or credential helper
- GPG uses gpg-agent

---

## Documentation Assumptions

### 20. Comprehensive Help Text

**Assumption**: `--help` shows full usage with examples.

**Rationale**:
- User experience best practice
- Reduces support burden
- Self-documenting CLI

---

## Testing Assumptions

### 21. Manual Testing Workflow

**Assumption**: No automated test suite initially; testing is manual with real tools.

**Rationale**:
- Deployment guide didn't mention tests
- Integration testing requires GPG, Git, OTS setup
- Initial focus on working implementation

**TODO**: Add test suite with mocked external commands.

---

## Known Limitations

### Areas Requiring User Input

1. **Palimpsest License Text**: Custom license mentioned but text unknown
2. **Git Remote URLs**: Hardcoded placeholders in Publisher module
3. **TOML Parsing**: Configuration parsing not implemented
4. **Wikidata Integration**: Deployment guide mentioned but not implemented
5. **Claim File Template**: Format assumed, may need customization

### Implementation Gaps

1. **Error Recovery**: Limited rollback on partial failures
2. **Transaction Safety**: No atomic commit of all operations
3. **Logging**: Basic stdout logging, no structured logs
4. **Internationalization**: English-only
5. **Windows Support**: Assumes Unix-like environment

---

## Validation Needed

The following should be validated against original design intent:

- [ ] Claim file structure and legal sufficiency
- [ ] Git workflow (branch names, commit format, tag naming)
- [ ] Remote repository configuration approach
- [ ] Error handling philosophy (fail fast vs graceful degradation)
- [ ] Configuration file format and default values
- [ ] Monitor script functionality and metrics
- [ ] Container deployment approach

---

## Reconstruction Confidence Levels

| Component | Confidence | Notes |
|-----------|------------|-------|
| Makefile | 95% | Standard build patterns |
| GNAT Project | 95% | Standard Ada project |
| Containerfile | 90% | Standard container build |
| GitLab CI/CD | 90% | Standard pipeline |
| SaltStack State | 85% | Standard deployment |
| CLI Parsing | 80% | Assumed flag names |
| Workflow Logic | 75% | Assumed orchestration |
| Claim Format | 60% | Assumed Markdown structure |
| Config Format | 50% | Assumed TOML, not parsed |
| Palimpsest License | 0% | Unknown custom license |

---

## Recommendations for Review

When reviewing this reconstruction:

1. **Validate Claim File Format**: Ensure legal sufficiency
2. **Provide Palimpsest License**: Add full license text
3. **Configure Git Remotes**: Update Publisher module with actual remote URLs
4. **Test Cryptographic Chain**: Verify GPG + OTS + Git signatures end-to-end
5. **Review Error Handling**: Decide on fail-fast vs graceful degradation policy
6. **Add Wikidata Integration**: If desired in original design
7. **Implement TOML Parsing**: If configuration files are needed

---

## Change Log

- **2025-11-22**: Initial reconstruction from deployment guide
  - All core modules implemented
  - Infrastructure complete (build, container, CI/CD)
  - Documentation added
  - Ready for validation and refinement

---

*This document should be updated as original design details are recovered or validated.*
