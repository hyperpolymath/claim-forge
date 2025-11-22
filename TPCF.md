# Tri-Perimeter Contribution Framework (TPCF)

**Version**: 1.0
**Project**: Claim Forge
**Last Updated**: 2025-11-22

---

## Overview

The **Tri-Perimeter Contribution Framework** (TPCF) is a graduated trust model for open-source contributions. It addresses the central tension in open source: how to be **radically open** to new contributors while maintaining **security and quality**.

### The Problem

Traditional contribution models face challenges:

- **Fully Open**: Anyone can commit → Security risks, spam, low-quality code
- **Fully Closed**: Only maintainers can commit → Slow, exclusive, discourages contribution
- **Binary Trust**: You're either "in" or "out" → No growth path for new contributors

### The TPCF Solution

**Graduated Trust with Three Perimeters**:

```
┌─────────────────────────────────────────────────┐
│ Perimeter 3: Community Sandbox (Public)        │ ← Everyone
│  Trust: Low | Access: Fork, PR, Issues         │
│                                                 │
│  ┌───────────────────────────────────────────┐ │
│  │ Perimeter 2: Trusted Contributors         │ ← Proven track record
│  │  Trust: Medium | Access: Feature branches │ │
│  │                                            │ │
│  │  ┌─────────────────────────────────────┐  │ │
│  │  │ Perimeter 1: Core Maintainers       │  │ ← Established trust
│  │  │  Trust: High | Access: Main, merge  │  │ │
│  │  │                                      │  │ │
│  │  │  [Main Branch, Releases, Security]  │  │ │
│  │  └─────────────────────────────────────┘  │ │
│  └───────────────────────────────────────────┘ │
└─────────────────────────────────────────────────┘
```

### Core Principles

1. **Reversibility**: All code changes are reversible (Git history)
2. **Transparency**: Clear rules for advancement between perimeters
3. **Meritocracy**: Earn trust through contribution quality, not politics
4. **Emotional Safety**: Perimeter 3 is explicitly experimental/learning
5. **No Gatekeeping**: Path from P3 → P2 → P1 is clearly documented

---

## Perimeter Levels

### Perimeter 3: Community Sandbox

**Who**: Anyone with a GitHub account

**Philosophy**: **Radically open**. This is the experimental zone where new contributors learn, make mistakes, and grow. Anxiety should be **low** here.

#### Access Rights

- ✅ Read entire repository
- ✅ Fork repository
- ✅ Create issues
- ✅ Submit pull requests
- ✅ Comment on issues/PRs
- ✅ Participate in GitHub Discussions
- ❌ Direct commit to any branch
- ❌ Merge pull requests

#### Contribution Workflow

1. **Fork** the repository to your own account
2. **Branch** from main: `git checkout -b feature/your-feature`
3. **Commit** your changes: `git commit -s -m "feat: add feature"`
4. **Push** to your fork: `git push origin feature/your-feature`
5. **Pull Request** to main repository
6. **Code Review** by P2 or P1 maintainer
7. **Iteration** based on feedback
8. **Merge** by P2/P1 maintainer (if approved)

#### Quality Expectations

- **Compiles**: Code must compile with GNAT
- **Tests Pass**: `make && docs/examples/test-workflow.sh` must pass
- **Follows Style**: Ada 2012 conventions (3-space indent, Ada_Case)
- **Documented**: Comments for non-obvious logic
- **Conventional Commits**: Use `feat:`, `fix:`, `docs:` prefixes

**Important**: P3 contributions may be **imperfect**. That's OK! Reviewers will help improve them.

#### Emotional Safety Guarantees

- ✅ **Safe to Experiment**: Try unconventional approaches
- ✅ **Safe to Ask**: No question is "stupid"
- ✅ **Safe to Fail**: Failed PRs are learning experiences
- ✅ **Safe to Revise**: Iteration is encouraged, not shameful
- ❌ **Not Safe to Harass**: CODE_OF_CONDUCT.md fully applies

#### Advancement to P2

Automatic after:
- ✅ **3+ merged pull requests** (non-trivial, e.g., not just typo fixes)
- ✅ **3+ months of active participation** (issues, discussions, code review)
- ✅ **Zero Code of Conduct violations**
- ✅ **Technical competence demonstrated** (code quality improves over time)

**How to Claim**: Comment on issue requesting P2 access, link to 3 PRs

---

### Perimeter 2: Trusted Contributors

**Who**: Contributors with proven track record (promoted from P3)

**Philosophy**: **Earn more autonomy**. P2 contributors have demonstrated competence and alignment with project values. They get faster feedback loops and can help onboard P3 contributors.

#### Access Rights

- ✅ All P3 rights
- ✅ **Direct commit** to feature branches (not main)
- ✅ **Create branches** in main repository (no need to fork)
- ✅ **Review pull requests** (advisory, not final approval)
- ✅ **Triage issues** (label, assign, close duplicates)
- ✅ **Access to P2 discussion channel** (GitHub Discussions, private section)
- ❌ Merge to main branch
- ❌ Create releases
- ❌ Access to security advisories

#### Contribution Workflow

1. **Branch** directly in main repo: `git checkout -b feature/your-feature`
2. **Commit** and push to origin (not fork)
3. **Pull Request** to main
4. **Self-Review**: Can approve own PR if minor (typos, docs)
5. **Peer Review**: Major changes need P1 or another P2 review
6. **Merge**: P1 maintainer merges (or P2 with delegation)

#### Responsibilities

- **Code Review**: Help review P3 pull requests
- **Mentorship**: Answer P3 questions in issues/discussions
- **Issue Triage**: Label and organize issues
- **Documentation**: Keep docs up-to-date
- **Quality**: Maintain higher standard than P3 (set example)

#### Quality Expectations

- **Higher Bar**: Code should be production-ready on first PR
- **Tests**: Include tests for new features
- **Documentation**: Update relevant docs in same PR
- **Breaking Changes**: Discuss in issue before implementing
- **Security**: Consider security implications of changes

#### Advancement to P1

Requires:
- ✅ **6+ months as P2**
- ✅ **20+ merged pull requests** (including as P2)
- ✅ **Demonstrated leadership**: Design docs, mentoring, architecture discussions
- ✅ **Unanimous approval** from existing P1 maintainers
- ✅ **Commitment**: Able to dedicate 3-5 hours/week

**How to Nominate**: Current P1 nominates via email to team@hyperpolymath.example

---

### Perimeter 1: Core Maintainers

**Who**: Project maintainers (see [MAINTAINERS.md](MAINTAINERS.md))

**Philosophy**: **High trust, high responsibility**. P1 maintainers are stewards of the project. They make final decisions, manage releases, and handle security incidents.

#### Access Rights

- ✅ All P2 rights
- ✅ **Merge to main branch**
- ✅ **Create and push tags**
- ✅ **Create releases**
- ✅ **Manage GitHub repository settings**
- ✅ **Access to security advisories** (private disclosures)
- ✅ **Add/remove collaborators** (P2 access)
- ✅ **Voting rights** on project decisions

#### Responsibilities

- **Final Approval**: Merge pull requests to main
- **Release Management**: Create and publish releases
- **Security**: Respond to security incidents within 48 hours
- **Code of Conduct**: Enforce CODE_OF_CONDUCT.md
- **Governance**: Participate in maintainer votes
- **Availability**: Respond to critical issues within 48 hours
- **Mentorship**: Onboard new P2 and P1 members
- **Vision**: Shape project roadmap and architecture

#### Decision-Making Powers

- **Lazy Consensus**: For non-controversial changes, 7-day silence = approval
- **Voting**: For significant decisions (2/3 majority required)
- **Veto**: Project Lead has veto power (rarely used)

#### Time Commitment

- **Expected**: 5-10 hours/week for Project Lead, 3-5 hours/week for others
- **Leave Policy**: Can take leave with 2-week notice (see MAINTAINERS.md)
- **Stepping Down**: Always honored with gratitude (no shame!)

---

## Contribution Types and Perimeter Requirements

| Contribution Type | P3 | P2 | P1 | Notes |
|-------------------|----|----|-----|-------|
| Report issue | ✅ | ✅ | ✅ | Anyone can report bugs/features |
| Comment on issue | ✅ | ✅ | ✅ | Discussions are open |
| Submit PR (fork) | ✅ | ✅ | ✅ | P3 workflow |
| Submit PR (branch) | ❌ | ✅ | ✅ | P2+ can branch directly |
| Review PR (advisory) | ❌ | ✅ | ✅ | P2 reviews are non-binding |
| Approve PR | ❌ | ⚠️ | ✅ | P2 can approve minor PRs |
| Merge to feature branch | ❌ | ✅ | ✅ | P2+ for their own branches |
| Merge to main | ❌ | ❌ | ✅ | P1 only |
| Create release | ❌ | ❌ | ✅ | P1 only |
| Triage issues | ❌ | ✅ | ✅ | Label, close, assign |
| Security advisory | ❌ | ❌ | ✅ | P1 only (sensitive) |

---

## Why Graduated Trust Works

### Traditional Model Problems

**Binary Trust**:
```
Outsider → [HIGH BARRIER] → Core Team
    ↑                            ↑
  Discouraged              Overwhelmed
  No clear path          All responsibility
```

### TPCF Solution

**Graduated Path**:
```
P3 (Experimental) → [3 PRs] → P2 (Trusted) → [6 months] → P1 (Core)
     ↑                           ↑                           ↑
  Low barrier            Proven competence          Proven leadership
  Learning zone          More autonomy              Full responsibility
```

### Benefits

#### For New Contributors (P3)

- **Low barrier to entry**: Just submit a PR!
- **Safe learning environment**: Mistakes are expected
- **Clear advancement path**: 3 PRs + 3 months → P2
- **No politics**: Merit-based advancement

#### For Trusted Contributors (P2)

- **Faster feedback loops**: No need to fork
- **More autonomy**: Can create branches, review PRs
- **Mentorship**: Help onboard new P3 contributors
- **Recognition**: Status as trusted community member

#### For Core Maintainers (P1)

- **Scalability**: P2 members reduce P1 workload
- **Quality**: P2 members pre-filter PRs
- **Succession**: Clear pipeline for new P1 maintainers
- **Security**: Separate untrusted (P3) from trusted (P2/P1) code

#### For the Project

- **Openness**: Anyone can contribute (P3)
- **Quality**: Graduated trust maintains standards
- **Growth**: Clear path for contributor → maintainer
- **Sustainability**: Avoids single-point-of-failure (many P1s)

---

## Emotional Safety Integration

TPCF complements [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) with structural safety:

### P3: Experimental Zone

- **Anxiety Reduction**: Failure is normalized, expected, and valuable
- **Reversibility**: All changes via PR, easily reverted if needed
- **Psychological Safety**: Ask questions, try unconventional ideas, iterate openly

### P2: Mentorship Zone

- **Responsibility**: P2 members model emotional safety for P3
- **Growth**: Support P3 contributors with patience and empathy
- **Balance**: Maintain quality while encouraging experimentation

### P1: Exemplar Zone

- **Leadership**: Set tone for community interactions
- **Enforcement**: Uphold Code of Conduct fairly and compassionately
- **Vulnerability**: Model admitting mistakes and asking for help

---

## Measuring Success

TPCF success metrics (tracked quarterly):

| Metric | Target | Rationale |
|--------|--------|-----------|
| P3 → P2 conversion rate | > 10% | Shows clear advancement path |
| P2 retention (6 months) | > 75% | P2 members stay engaged |
| P3 experiment rate | > 40% | Encourages trying new ideas |
| Code of Conduct violations | < 2% | Healthy community culture |
| Time to first PR merge | < 7 days | Responsive to new contributors |

---

## Comparison to Other Models

### TPCF vs. Traditional Open Source

| Aspect | Traditional | TPCF |
|--------|-------------|------|
| Entry Barrier | Low (anyone can PR) | Low (anyone can PR) |
| Merge Rights | Core team only | Graduated (P2 has some) |
| Advancement Path | Unclear, political | Clear, merit-based |
| Security | Binary (trusted or not) | Graduated (P3 < P2 < P1) |

### TPCF vs. Corporate Gatekeeping

| Aspect | Corporate | TPCF |
|--------|-----------|------|
| Entry Barrier | High (employee only) | Low (anyone can contribute) |
| Contributor Rights | CLA required | No CLA, GPG signing |
| Advancement | Employment-based | Merit-based |
| Transparency | Opaque decisions | Public, documented process |

---

## Frequently Asked Questions

### How long does P3 → P2 take?

**Minimum**: 3 months + 3 PRs

**Average**: 3-6 months for active contributors

**Reality**: Some never advance (and that's OK! Casual contribution is valuable too)

### Can I skip P3 and go straight to P2?

**No**. Everyone starts at P3, even experienced developers. This:
- Demonstrates alignment with project values
- Shows sustained interest (not one-time contributor)
- Prevents fast-tracking based on status/fame

### What if I disagree with a P1 decision?

1. **Discuss**: Comment on issue/PR with reasoning
2. **Escalate**: Request vote if it's a significant decision
3. **Accept**: If vote doesn't go your way, accept decision gracefully
4. **Fork**: Open source allows forks if fundamentally incompatible

### Can I be demoted from P2 → P3?

**Rarely**, only for:
- **Inactivity**: > 6 months with no contributions (graceful demotion)
- **Code of Conduct**: Serious violations (with warning first)
- **Security**: Compromised account or suspicious activity

**NOT for**:
- Making mistakes in code (everyone makes mistakes)
- Disagreeing with P1 decisions (dissent is healthy)
- Taking a break (life happens!)

### How does TPCF handle security?

- **P3**: No access to security advisories (public internet, untrusted)
- **P2**: Still no access (need-to-know basis only)
- **P1**: Full access to security advisories, responsible for response

**Critical**: Security vulnerabilities are NEVER discussed in public issues, only in private GitHub Security Advisories.

---

## Implementation in Claim Forge

### Current Perimeter Assignments

- **P1**: Hyperpolymath (Project Lead)
- **P2**: *None yet - first P2 members will be nominated soon!*
- **P3**: Everyone else (you!)

### GitHub Setup

- **Branch Protection**: Main branch requires P1 approval
- **Review Requirements**: 1 approval for PRs to main
- **Status Checks**: CI must pass (build, tests)
- **Signed Commits**: Encouraged, not required for P3

### Tools

- **GitHub Teams**: `@claim-forge/p1-maintainers`, `@claim-forge/p2-contributors`
- **Labels**: `P3-friendly` for good first issues
- **Milestones**: Track advancement (`P3 → P2 nominees`)

---

## Further Reading

- [MAINTAINERS.md](MAINTAINERS.md) - Current maintainers and their roles
- [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) - Community interaction standards
- [CONTRIBUTING.md](CONTRIBUTING.md) - How to contribute code
- [SECURITY.md](SECURITY.md) - Security policy and reporting

---

## Credits

TPCF was inspired by:
- Rust Project's tiered membership model
- Kubernetes SIG structure
- Creative Commons graduated permissions
- CCCP Manifesto (Emotional Safety in Open Source)

---

*Version: 1.0*
*Last Updated: 2025-11-22*
*License: CC BY 4.0*
