# CLAUDE.md

## Project Overview

**claim-forge** is a project for managing and processing claims.

## Development Setup

### Prerequisites

List the required tools and versions needed to work on this project:
- Node.js (specify version)
- Package manager (npm, yarn, pnpm)
- Any other dependencies

### Installation

```bash
# Clone the repository
git clone https://github.com/Hyperpolymath/claim-forge.git
cd claim-forge

# Install dependencies
npm install

# Set up environment variables
cp .env.example .env
```

### Running the Project

```bash
# Development
npm run dev

# Build
npm run build

# Test
npm test
```

## Project Structure

```
claim-forge/
├── src/           # Source code
├── tests/         # Test files
├── docs/          # Documentation
└── ...
```

## Working with Claude Code

### Common Tasks

When working with Claude Code on this project, here are some common workflows:

#### Running Tests
```
Run the test suite and fix any failures
```

#### Adding Features
```
Add a new feature for [description]. Make sure to:
- Write tests
- Update documentation
- Follow existing code patterns
```

#### Debugging
```
Investigate why [feature] is not working as expected
```

#### Code Review
```
Review the changes in [file/directory] for potential issues
```

### Best Practices

1. **Always run tests** before committing changes
2. **Follow the existing code style** - maintain consistency with the current codebase
3. **Write clear commit messages** - use conventional commits format
4. **Update documentation** - keep docs in sync with code changes
5. **Ask before major refactors** - discuss significant architectural changes

### Useful Commands

```bash
# Lint code
npm run lint

# Format code
npm run format

# Type check
npm run type-check

# Run specific test
npm test -- [test-name]
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

- Use consistent indentation (2 or 4 spaces)
- Follow language-specific conventions
- Write self-documenting code with clear variable names
- Add comments for complex logic
- Keep functions small and focused

## Testing

- Write unit tests for new functionality
- Maintain or improve code coverage
- Test edge cases and error conditions
- Use descriptive test names

## Documentation

- Update README.md for user-facing changes
- Document public APIs
- Add inline comments for complex logic
- Keep this CLAUDE.md updated with new workflows

## Troubleshooting

### Common Issues

**Issue**: [Common problem]
**Solution**: [How to fix]

## Resources

- [Project Documentation](./docs)
- [Contributing Guidelines](./CONTRIBUTING.md)
- [Code of Conduct](./CODE_OF_CONDUCT.md)

## Notes for Claude

- Always check for existing patterns before implementing new features
- Prefer editing existing files over creating new ones
- Ask for clarification if requirements are ambiguous
- Run tests after making changes
- Keep commits atomic and well-described

---

*Last updated: 2025-11-21*
