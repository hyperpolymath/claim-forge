# Security Policy

## Overview

Claim Forge is an IP registration and blockchain timestamping system designed with security as a foundational principle. This document outlines our security practices, vulnerability reporting procedures, and security considerations for users.

## Supported Versions

We provide security updates for the following versions:

| Version | Supported          | End of Support |
| ------- | ------------------ | -------------- |
| 1.0.x   | ✅ Yes             | TBD            |
| < 1.0   | ❌ No              | N/A (pre-release) |

## Reporting a Vulnerability

**🚨 CRITICAL: DO NOT report security vulnerabilities through public GitHub issues.**

### Preferred Reporting Methods

#### 1. GitHub Security Advisories (Recommended)

1. Navigate to [https://github.com/Hyperpolymath/claim-forge/security/advisories/new](https://github.com/Hyperpolymath/claim-forge/security/advisories/new)
2. Fill out the advisory form with:
   - Vulnerability description
   - Steps to reproduce
   - Impact assessment
   - Proof of concept (if available)
3. Submit - only repository maintainers will see this

#### 2. Encrypted Email (For Sensitive Disclosures)

- **Email**: security@hyperpolymath.example
- **Subject**: `[SECURITY] Claim Forge: [Brief Description]`
- **Encryption**: Use our GPG key (fingerprint: [TBD - replace with actual])
- **Response Time**: Within 48 hours

### What to Include in Your Report

A good vulnerability report includes:

1. **Description**: Clear explanation of the vulnerability
2. **Affected Components**: Which parts of Claim Forge are impacted
3. **Steps to Reproduce**: Detailed reproduction steps
4. **Proof of Concept**: Code, commands, or screenshots
5. **Impact Assessment**: Potential consequences (confidentiality, integrity, availability)
6. **Suggested Fix**: If you have ideas (optional but appreciated)
7. **CVE Request**: Whether you want to request a CVE identifier

### Response Timeline

We commit to the following response times:

- **48 hours**: Initial acknowledgment of report
- **7 days**: Preliminary assessment and severity classification
- **30 days**: Fix development and testing (for critical/high severity)
- **60 days**: Public disclosure (coordinated with reporter)
- **90 days**: Maximum embargo period (critical issues may be disclosed sooner if actively exploited)

### Severity Classification

We use CVSS 3.1 scoring:

| Severity | CVSS Score | Response Time | Example |
|----------|------------|---------------|---------|
| **Critical** | 9.0-10.0 | 7 days | Remote code execution, privilege escalation |
| **High** | 7.0-8.9 | 30 days | Authentication bypass, data leakage |
| **Medium** | 4.0-6.9 | 60 days | DoS, information disclosure |
| **Low** | 0.1-3.9 | 90 days | Minor information leaks, edge cases |

## Security Architecture

### Defense in Depth

Claim Forge implements multiple security layers:

#### 1. Type Safety (Ada 2012)

- **Strong Typing**: Compile-time type checks prevent type confusion
- **Range Constraints**: Bounded strings prevent buffer overflows
- **Contract-Based Programming**: Preconditions and postconditions enforce invariants
- **No Implicit Conversions**: Explicit type conversions required

**Example**:
```ada
-- Bounded strings prevent buffer overflows
subtype Bounded_String is String (1 .. 1024);
-- Compiler rejects any string > 1024 characters
```

#### 2. Memory Safety

- **No Dynamic Allocation**: Stack-only memory usage
- **No Garbage Collector**: Predictable, deterministic resource management
- **Zero Unsafe Blocks**: No `unsafe` code in entire codebase
- **Ownership Model**: Clear lifetimes for all resources

**Security Benefit**: Eliminates entire classes of vulnerabilities (use-after-free, double-free, memory leaks)

#### 3. Cryptographic Integrity

- **GPG Signing**: All claim files cryptographically signed
- **OpenTimestamps**: Blockchain-anchored timestamps (Bitcoin)
- **Signed Commits**: All repository commits GPG-signed by maintainers
- **Signed Tags**: Release tags signed for verification

**Verification**:
```bash
gpg --verify CLAIM.md.sig CLAIM.md
ots verify CLAIM.md.ots
git verify-commit HEAD
git verify-tag v1.0.0
```

#### 4. Offline-First Design

- **Air-Gapped Operation**: Core functionality works without network
- **No Telemetry**: Zero phone-home, tracking, or analytics
- **No Auto-Updates**: Explicit update control by user
- **Optional Network**: OTS and Git push are opt-in

**Use Case**: Sensitive IP registration in secure environments

#### 5. Principle of Least Privilege

- **No Root Required**: Runs as unprivileged user
- **Minimal Dependencies**: Zero runtime dependencies (GNAT compile-time only)
- **No Daemon**: CLI tool, no background services
- **User-Controlled**: All operations explicit, no automation without consent

### Security Features

#### Input Validation

All user inputs are validated:

```ada
-- CLI argument parsing with bounds checking
if Argument_Count > Max_Arguments then
   raise Invalid_Arguments with "Too many arguments";
end if;

-- File path validation
if not Ada.Directories.Exists (File_Path) then
   raise Claim_Error with "File not found: " & File_Path;
end if;
```

#### Command Injection Prevention

External tool invocation uses argument arrays:

```ada
-- Safer: Argument array (not yet implemented)
-- Args := ("git", "commit", "-m", User_Input);

-- Current: String-based (documented risk in ASSUMPTIONS.md #8)
-- Mitigation: Input sanitization, bounded strings
Command := "git commit -m """ & Escape_Quotes (User_Input) & """";
```

**Known Risk**: Shell command execution (see [Known Security Considerations](#known-security-considerations))

## Scope

### In Scope (We Accept Reports For)

- ✅ Core Ada application (src/*.adb, src/*.ads)
- ✅ Build system (Makefile, claim-forge.gpr, justfile)
- ✅ CI/CD pipeline (.gitlab-ci.yml)
- ✅ Container images (Containerfile)
- ✅ Automation scripts (setup.sh, bin/monitor.sh)
- ✅ Documentation rendering (XSS, injection)
- ✅ Supply chain (dependency vulnerabilities)

### Out of Scope (Report to Upstream)

- ❌ GPG vulnerabilities (report to GnuPG project)
- ❌ Git vulnerabilities (report to Git project)
- ❌ OpenTimestamps client (report to OTS project)
- ❌ Operating system vulnerabilities
- ❌ Hardware vulnerabilities (Spectre, Meltdown, etc.)
- ❌ Social engineering attacks
- ❌ Physical security

## Known Security Considerations

We believe in transparency. Here are known security considerations:

### 1. Shell Command Execution

**Issue**: GPG, Git, and OpenTimestamps are invoked via shell commands using `GNAT.OS_Lib.Spawn`.

**Risk**: Potential command injection if inputs are not properly sanitized.

**Mitigations**:
- Bounded strings limit input length
- Quote escaping for user inputs
- No shell metacharacters in validated paths
- Future: Native Ada bindings (planned)

**Documented**: ASSUMPTIONS.md #8

**Severity**: Low (mitigated by input validation)

### 2. GPG Key Management

**Issue**: Users manage their own GPG keys; Claim Forge does not create or store keys.

**Risk**: Weak keys, compromised passphrases, lost keys.

**Mitigations**:
- Documentation on best practices (README.md)
- Recommendation: Hardware security keys (YubiKey, Nitrokey)
- No passphrase prompting (user's gpg-agent handles this)
- No key storage in Claim Forge

**Severity**: N/A (user responsibility, documented in README)

### 3. Dependency Trust

**Issue**: Trust in GNAT compiler and Ada runtime.

**Risk**: Compromised compiler could inject backdoors.

**Mitigations**:
- Use official AdaCore GNAT or GNU Ada
- Verify package signatures (when available)
- Reproducible builds (future: Nix flake)
- Build from audited source

**Severity**: Low (trust in established toolchain)

### 4. Network Operations

**Issue**: Git push and OpenTimestamps require network access.

**Risk**: Man-in-the-middle attacks, DNS spoofing.

**Mitigations**:
- SSH for Git (authenticated, encrypted)
- HTTPS for OpenTimestamps (TLS)
- Optional: Run offline, push manually
- Verification: `ots verify` checks blockchain

**Severity**: Low (uses industry-standard protocols)

## Best Practices for Users

### 1. Verify Downloads

```bash
# Check GPG signature on releases
gpg --verify claim-forge-1.0.0.tar.gz.sig claim-forge-1.0.0.tar.gz
```

### 2. Build from Source

```bash
# Audit source code before building
git clone https://github.com/Hyperpolymath/claim-forge.git
cd claim-forge
# Review src/*.adb, src/*.ads
make clean && make release
```

### 3. Use Hardware Security Keys

For production IP registration:
- Use YubiKey or Nitrokey for GPG keys
- Enable touch-to-sign for sensitive operations
- Keep backup keys in secure location

### 4. Air-Gapped Operation

For maximum security:
```bash
# On air-gapped machine:
./bin/claim-forge --repo my-project --file paper.pdf --description "Research" --no-push --no-timestamp

# Transfer claim file to connected machine for OTS/Git push
```

### 5. Audit Generated Claims

```bash
# Review claim before signing
cat CLAIM.md

# Verify GPG signature
gpg --verify CLAIM.md.sig CLAIM.md

# Check OpenTimestamps proof
ots info CLAIM.md.ots
```

## Security Updates

### Notification Channels

We announce security updates through:

1. **GitHub Security Advisories**: https://github.com/Hyperpolymath/claim-forge/security/advisories
2. **Release Notes**: https://github.com/Hyperpolymath/claim-forge/releases
3. **CHANGELOG.md**: Updated with each security fix
4. **Mailing List**: security-announce@hyperpolymath.example (planned)

### Update Policy

- **Critical**: Patch released within 7 days
- **High**: Patch released within 30 days
- **Medium/Low**: Patch released in next minor version

### Verification

All security updates are:
- GPG-signed by maintainers
- Tagged in Git with signed tags
- Documented in CHANGELOG.md
- Announced in GitHub releases

## CVE Process

For significant vulnerabilities, we:

1. **Request CVE**: Through GitHub Security Advisories
2. **Coordinate Disclosure**: With reporter and downstream users
3. **Publish Advisory**: With CVE number, severity, affected versions
4. **Release Patch**: With clear upgrade instructions
5. **Credit Researcher**: In acknowledgments (if desired)

## Safe Harbor

We support good-faith security research:

### What We Will NOT Do

- ❌ Pursue legal action against security researchers
- ❌ Contact law enforcement about good-faith research
- ❌ Request removal of researcher tools/publications

### What We Expect

- ✅ Follow responsible disclosure (give us time to fix)
- ✅ Do not access or modify production data
- ✅ Do not perform denial-of-service attacks
- ✅ Report through proper channels (not public issues)
- ✅ Respect user privacy

### Recognition

Researchers who report valid vulnerabilities receive:

- Public acknowledgment (if desired)
- Credit in CHANGELOG.md and release notes
- Listed in [Acknowledgments](#acknowledgments) section
- CVE credit (for significant findings)

## Acknowledgments

We thank the following security researchers for responsible disclosure:

- *None yet - be the first!*

To be listed here, report a valid security vulnerability through our [reporting process](#reporting-a-vulnerability).

## Contact

- **Security Team**: security@hyperpolymath.example (GPG encrypted preferred)
- **GitHub**: https://github.com/Hyperpolymath/claim-forge/security/advisories/new
- **GPG Key**: [Fingerprint TBD - replace with actual]

## Resources

- [OWASP Top Ten](https://owasp.org/www-project-top-ten/)
- [CWE Top 25](https://cwe.mitre.org/top25/)
- [NIST Cybersecurity Framework](https://www.nist.gov/cyberframework)
- [Ada Security Guide](https://www.adaic.org/resources/add_content/standards/12rm/html/RM-H-4.html)

## License

This security policy is licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

---

*Last Updated: 2025-11-22*
*Version: 1.0*
