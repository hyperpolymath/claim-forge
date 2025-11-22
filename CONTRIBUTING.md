# Contributing to Claim Forge

Thank you for considering contributing to Claim Forge! This document provides guidelines for contributing to this Ada-based IP registration system.

## Code of Conduct

- Be respectful and constructive
- Focus on technical merit
- Help others learn Ada and cryptographic best practices
- Report security issues privately

## Getting Started

### Prerequisites

- GNAT compiler 2021+
- Git
- GPG
- Python 3.8+ with opentimestamps-client
- Basic Ada knowledge (or willingness to learn!)

### Development Setup

```bash
# Fork and clone
git clone https://github.com/YOUR_USERNAME/claim-forge.git
cd claim-forge

# Run setup
./setup.sh

# Build
make clean && make debug

# Test
./docs/examples/test-workflow.sh
```

## How to Contribute

### Reporting Bugs

1. **Search existing issues** first
2. **Provide details**:
   - Claim Forge version (`./bin/claim-forge --version`)
   - GNAT version (`gnat --version`)
   - Operating system
   - Steps to reproduce
   - Expected vs actual behavior
   - Error messages/logs

Example:

```markdown
**Version**: v1.0.0
**GNAT**: GNAT 12.2.0
**OS**: Fedora 38

**Steps to Reproduce**:
1. Run `claim-forge --repo test --file test.pdf --description "Test"`
2. Observe error

**Expected**: Claim created successfully
**Actual**: GPG signing fails with error "secret key not available"
```

### Suggesting Features

1. **Check existing issues/PRs** for duplicates
2. **Describe the use case** clearly
3. **Explain the benefit** to users
4. **Consider implementation complexity**

### Contributing Code

#### 1. Find/Create an Issue

- Comment on existing issue or create new one
- Discuss approach before major work
- Get feedback on design

#### 2. Create a Branch

```bash
git checkout -b feature/your-feature-name
# or
git checkout -b fix/bug-description
```

#### 3. Make Changes

Follow the code style guidelines below.

#### 4. Test Your Changes

```bash
# Build in debug mode
make debug

# Run tests
./docs/examples/test-workflow.sh

# Test your specific changes manually

# Check syntax
make check
```

#### 5. Commit

```bash
# Use conventional commits format
git commit -S -m "feat: add support for custom claim templates"
git commit -S -m "fix: handle missing GPG key gracefully"
git commit -S -m "docs: update deployment guide"
```

Commit types:
- `feat:` New feature
- `fix:` Bug fix
- `docs:` Documentation
- `test:` Tests
- `refactor:` Code refactoring
- `chore:` Maintenance

#### 6. Push and Create PR

```bash
git push -u origin feature/your-feature-name
```

Then create a Pull Request on GitHub with:
- Clear title and description
- Link to related issue
- Screenshots/examples if applicable
- Explanation of changes

## Code Style Guidelines

### Ada Style

Follow the Ada style in existing code:

```ada
-- Good: 3-space indentation
procedure Example is
begin
   if Condition then
      Do_Something;
   end if;
end Example;

-- Good: Ada_Case for identifiers
type Config_Type is record
   Default_License : Bounded_String;
end record;

-- Good: Descriptive names
procedure Generate_Claim_File (
   Claim_File  : in String;
   Description : in String
);

-- Bad: Unclear abbreviations
procedure Gen_CF (CF : in String; Desc : in String);
```

### Specific Rules

1. **Indentation**: 3 spaces (not tabs)
2. **Naming**:
   - Types: `Config_Type`, `Options_Type`
   - Procedures: `Generate_Claim_File`
   - Functions: `Get_Default_Config`
   - Constants: `Max_String_Length`
3. **Line Length**: Max 100 characters
4. **Comments**: Use `--` for single-line comments
5. **One package per file pair** (`.ads` and `.adb`)

### Type Safety

Prioritize type safety:

```ada
-- Good: Strong typing
type Repo_Name is new String (1 .. 256);
procedure Set_Repo (Name : in Repo_Name);

-- Acceptable: Bounded strings with validation
procedure Set_Repo (Name : in Bounded_String) with
   Pre => Name'Length <= Max_Repo_Length;

-- Avoid: Unbounded types without validation
procedure Set_Repo (Name : in String);  -- What's the max length?
```

### Error Handling

Use exceptions for exceptional conditions:

```ada
-- Good: Clear exception with context
if not File_Exists (Path) then
   raise Claim_Error with "File not found: " & Path;
end if;

-- Good: Domain-specific exceptions
GPG_Error : exception;
Git_Error : exception;

-- Bad: Generic exceptions
raise Program_Error;  -- Too vague
```

## Testing Guidelines

### Unit Tests

(TODO: Test framework to be added)

```ada
-- Future: Ada unit tests
procedure Test_Claim_Generation is
begin
   -- Setup
   -- Execute
   -- Assert
end Test_Claim_Generation;
```

### Integration Tests

Test with real tools:

```bash
# Create test script in docs/examples/
./docs/examples/test-your-feature.sh
```

### Manual Testing Checklist

- [ ] Build in debug mode succeeds
- [ ] Build in release mode succeeds
- [ ] `--help` shows correct info
- [ ] Interactive mode works
- [ ] Command-line mode works
- [ ] GPG signing works (if implemented)
- [ ] OpenTimestamps works (if implemented)
- [ ] Git operations work (if implemented)
- [ ] Error handling is graceful
- [ ] Logging is informative

## Documentation

### Code Documentation

```ada
-- Module: Claim File Generator
-- Purpose: Creates structured claim files in Markdown format
--
-- This package generates IP claim files with metadata including
-- timestamps, descriptions, and license information.

package Claim is

   -- Generate a claim file for the specified work
   --
   -- Parameters:
   --   Claim_File  : Output file path for claim
   --   File_Path   : Path to work being claimed
   --   Description : Human-readable description
   --   License     : License type (e.g., "Palimpsest")
   --
   -- Raises:
   --   Claim_Error : If file creation fails
   --
   procedure Generate (
      Claim_File  : in String;
      File_Path   : in String;
      Description : in String;
      License     : in String
   );

end Claim;
```

### User Documentation

- Update README.md for user-facing changes
- Add examples to docs/examples/
- Update ASSUMPTIONS.md if design decisions change
- Update CLAUDE.md for Claude Code workflows

## Reconstruction Context

This project was reconstructed from a deployment guide. When contributing:

1. **Review ASSUMPTIONS.md** to understand decisions made
2. **Document new assumptions** if original behavior is unknown
3. **Prefer clarity over cleverness** (it's a legal tool!)
4. **Ask questions** about original design intent

## Security Considerations

Claim Forge deals with cryptography and legal claims:

1. **Never store secrets** (GPG keys, passwords, etc.)
2. **Validate all inputs** (especially shell commands)
3. **Avoid command injection** in shell calls
4. **Document security implications** of changes
5. **Report vulnerabilities privately** to maintainers

### Example: Avoiding Command Injection

```ada
-- Bad: Vulnerable to injection
Command := "git commit -m " & User_Input;

-- Good: Properly escaped
Command := "git commit -m """ & Escape_Quotes (User_Input) & """";

-- Better: Use argument arrays (if available)
Args := ("git", "commit", "-m", User_Input);
```

## Review Process

1. **Automated checks** run on PR
2. **Maintainer review** for code quality
3. **Testing** by reviewer
4. **Feedback** and iteration
5. **Merge** when approved

## Community

- Be patient with review time
- Help review others' PRs
- Share knowledge about Ada
- Improve documentation

## Questions?

- Open an issue for questions
- Tag with "question" label
- Check existing docs first

## License

By contributing, you agree your contributions will be licensed under the project's license.

---

Thank you for contributing to Claim Forge!
