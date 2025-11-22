# Maintainers

This document lists the maintainers of the Claim Forge project and outlines their responsibilities.

## Active Maintainers

### Core Team

| Name | GitHub | Role | Specialization | Since |
|------|--------|------|----------------|-------|
| Hyperpolymath | [@Hyperpolymath](https://github.com/Hyperpolymath) | Project Lead | Architecture, Ada, Cryptography | 2025-11 |

### Emeritus Maintainers

*None yet - this section will list former maintainers who have stepped down*

## Roles and Responsibilities

### Project Lead

**Responsibilities**:
- Overall project vision and roadmap
- Final decision on architectural changes
- Security incident response coordination
- Release management
- Community health and Code of Conduct enforcement

**Powers**:
- Merge to main branch
- Create releases
- Manage GitHub repository settings
- Add/remove maintainers

**Contact**: team@hyperpolymath.example

### Core Maintainer

**Responsibilities**:
- Code review for pull requests
- Issue triage and labeling
- Documentation maintenance
- CI/CD pipeline maintenance
- Mentoring new contributors

**Powers**:
- Merge to feature branches
- Create tags (non-release)
- Label and close issues
- Review and approve PRs

**Minimum Requirements**:
- 6+ months active contribution
- 20+ merged pull requests
- Demonstrated technical expertise in Ada or cryptography
- Commitment to Code of Conduct

### Area Maintainer

**Responsibilities**:
- Expertise in specific area (build system, docs, cryptography, etc.)
- Review PRs in their area
- Maintain area-specific documentation
- Mentor contributors in their specialty

**Areas**:
- **Build System** (Makefile, GPR, CI/CD): *Open*
- **Cryptography** (GPG, OpenTimestamps): *Open*
- **Documentation** (Markdown, examples): *Open*
- **Ada Runtime** (Language features, safety): *Open*
- **DevOps** (Containers, SaltStack): *Open*

## Becoming a Maintainer

### Pathway: Community Sandbox → Trusted Contributor → Maintainer

This follows our [Tri-Perimeter Contribution Framework](TPCF.md):

#### 1. Community Sandbox (Perimeter 3)

**Entry**: Anyone with GitHub account

**Contributions**:
- Report issues
- Submit pull requests
- Participate in discussions
- Improve documentation

**Goal**: Demonstrate consistent, quality contributions

#### 2. Trusted Contributor (Perimeter 2)

**Requirements**:
- 3+ merged pull requests
- 3+ months active participation
- Adherence to Code of Conduct
- Technical competence demonstrated

**Nomination**: Self-nomination or maintainer nomination via GitHub issue

**Powers**:
- Direct commit to feature branches
- Review permissions on PRs
- Issue triage

#### 3. Maintainer (Perimeter 1)

**Requirements**:
- 6+ months as Trusted Contributor
- 20+ merged pull requests
- Demonstrated leadership (mentoring, design docs, etc.)
- Unanimous approval from existing maintainers

**Nomination Process**:
1. Current maintainer nominates via private email to team@hyperpolymath.example
2. Nominee accepts nomination
3. 2-week discussion period among maintainers
4. Unanimous approval required
5. Public announcement via GitHub Discussions

**Powers**:
- Merge to main branch
- Create releases (with Project Lead approval)
- Security incident access
- Maintainer voting rights

## Decision Making

### Consensus-Seeking

We prefer **lazy consensus**:

1. **Proposal**: Anyone can propose changes via GitHub Issue or Discussion
2. **Discussion**: 7-day minimum discussion period
3. **Consensus**: If no objections, proposal is accepted
4. **Objections**: Addressed through discussion and compromise

### Voting (When Consensus Fails)

For significant decisions without consensus:

- **Quorum**: 2/3 of active maintainers must participate
- **Threshold**: 2/3 majority required to pass
- **Veto**: Project Lead has veto power (used sparingly)

**Voting Eligible Decisions**:
- Major architectural changes
- Dependency additions
- License changes
- Adding/removing maintainers

### Emergency Decisions

For security incidents or time-sensitive issues:

- Project Lead can make unilateral decisions
- Must be documented and explained within 48 hours
- Can be appealed through normal voting process

## Time Commitment

Maintainers are volunteers. Expected time commitments:

- **Project Lead**: 5-10 hours/week
- **Core Maintainer**: 3-5 hours/week
- **Area Maintainer**: 1-3 hours/week

**Important**: Life happens. Maintainers can take leave:

- **Short Leave** (< 1 month): Notify other maintainers, no action needed
- **Extended Leave** (1-6 months): Temporary "inactive" status
- **Stepping Down** (> 6 months): Move to Emeritus, celebrate contributions!

## Onboarding New Maintainers

When a new maintainer is added:

### Week 1: Access and Orientation

1. Add to GitHub team with appropriate permissions
2. Add to private maintainer channels
3. Introduction to other maintainers
4. Review of governance docs (this file, TPCF.md, CODE_OF_CONDUCT.md)

### Week 2-4: Mentorship

5. Pair with existing maintainer on PR reviews
6. Shadow on release process
7. Practice issue triage

### Month 2+: Independent Action

8. Conduct PR reviews independently
9. Triage issues independently
10. Participate in maintainer discussions

## Stepping Down

Maintainers can step down gracefully:

1. **Notify**: Email team@hyperpolymath.example with 2-week notice (if possible)
2. **Transition**: Help transition responsibilities to other maintainers
3. **Recognition**: Public thank-you in GitHub Discussions
4. **Emeritus**: Listed in Emeritus Maintainers section
5. **Return**: Always welcome to return (fast-track re-onboarding)

**No Shame**: We celebrate all contributions. Life changes, and that's OK!

## Conflict Resolution

### Among Maintainers

1. **Direct Communication**: Speak directly with other maintainer (preferred)
2. **Mediator**: Request another maintainer to mediate
3. **Project Lead**: Escalate to Project Lead for decision
4. **Voting**: Use voting process if needed

### With Contributors

1. **Code of Conduct**: All interactions follow CODE_OF_CONDUCT.md
2. **Assume Good Intent**: Start with assumption of good faith
3. **Document**: Keep written records of decisions
4. **Appeal Process**: Contributors can appeal decisions (see CODE_OF_CONDUCT.md)

## Inactive Maintainers

If a maintainer is unresponsive for > 6 months:

1. **Attempt Contact**: Email and GitHub mention
2. **Wait 2 Weeks**: Grace period for response
3. **Temporary Removal**: Remove permissions (reversible)
4. **Emeritus Status**: Move to Emeritus list with thanks
5. **Welcome Back**: Easy reinstatement if they return

## Contact

- **Public**: GitHub Issues/Discussions
- **Private**: team@hyperpolymath.example
- **Security**: security@hyperpolymath.example
- **Code of Conduct**: conduct@hyperpolymath.example

## Governance Evolution

This governance model may evolve. Significant changes require:

- Proposal via GitHub Discussion
- 4-week discussion period
- Unanimous maintainer approval
- Public announcement

---

*Version: 1.0*
*Last Updated: 2025-11-22*
*Based on: Rust Project Governance, TPCF Graduated Trust Model*
