# Claim Forge - Quick Start Guide

This guide walks through a complete claim registration workflow.

## Prerequisites Check

Before starting, verify you have all required tools:

```bash
# Check GNAT compiler
gnat --version

# Check Git
git --version

# Check GPG
gpg --version
gpg --list-secret-keys  # Should show at least one key

# Check OpenTimestamps
ots --version

# Check Claim Forge
./bin/claim-forge --version
```

## Example 1: Interactive Mode

The simplest way to use Claim Forge:

```bash
# Run in interactive mode
./bin/claim-forge

# You'll be prompted for:
# - Repository name: my-research
# - File to claim: paper.pdf
# - Description: Novel distributed consensus algorithm
# - License: Palimpsest

# Confirm and the system will:
# 1. Generate CLAIM.md
# 2. Sign with GPG
# 3. Create OpenTimestamps proof
# 4. Commit to Git
# 5. Create signed tag
# 6. Push to remotes
```

## Example 2: Command-Line Mode

For automation or scripting:

```bash
./bin/claim-forge \
  --repo my-research \
  --file paper.pdf \
  --description "Novel distributed consensus algorithm" \
  --license "Palimpsest"
```

## Example 3: Skip Remote Push

Register claim locally without pushing:

```bash
./bin/claim-forge \
  --repo my-project \
  --file design.md \
  --description "System architecture design" \
  --no-push
```

## Example 4: Skip Timestamping

For testing without blockchain anchoring:

```bash
./bin/claim-forge \
  --repo test-project \
  --file test.txt \
  --description "Test claim" \
  --no-timestamp \
  --no-push
```

## Verifying Your Claim

After registration, verify the claim:

### 1. Verify GPG Signature

```bash
gpg --verify CLAIM.md.sig CLAIM.md
```

Expected output:
```
gpg: Signature made [date]
gpg: Good signature from "Your Name <your@email.com>"
```

### 2. Verify OpenTimestamps

```bash
ots verify CLAIM.md.ots
```

Expected output (immediately after creation):
```
Pending confirmation in Bitcoin blockchain
```

Expected output (after ~10 minutes):
```
Success! Bitcoin block [number] attests existence as of [date]
```

### 3. Verify Git Signatures

```bash
# Verify last commit
git verify-commit HEAD

# List and verify tags
git tag -l "claim-*"
git verify-tag claim-my-research
```

## Monitoring

Check system health:

```bash
# One-time health check
./bin/monitor.sh health

# Continuous monitoring
./bin/monitor.sh monitor

# Verify timestamps
./bin/monitor.sh verify
```

## Complete Workflow Example

Here's a complete session:

```bash
# 1. Create a file to claim
echo "# My Novel Algorithm" > algorithm.md
echo "This is my original research." >> algorithm.md

# 2. Register the claim
./bin/claim-forge \
  --repo novel-algorithm \
  --file algorithm.md \
  --description "Original distributed consensus algorithm" \
  --license "Palimpsest"

# 3. Verify everything
gpg --verify CLAIM.md.sig CLAIM.md
git verify-commit HEAD
git verify-tag claim-novel-algorithm

# 4. Check OTS status (may be pending)
ots verify CLAIM.md.ots

# 5. View the claim
cat CLAIM.md

# 6. Check Git log
git log --show-signature

# 7. Push to remotes (if not done automatically)
git push origin main --tags
```

## Troubleshooting

### GPG Signing Fails

```bash
# Check if you have a GPG key
gpg --list-secret-keys

# If not, create one
gpg --full-generate-key

# Configure Git to use it
git config --global user.signingkey YOUR_KEY_ID
git config --global commit.gpgsign true
```

### OpenTimestamps Fails

```bash
# Install OpenTimestamps client
pip3 install opentimestamps-client

# Verify it's in PATH
which ots
```

### Git Push Fails

```bash
# Check remotes
git remote -v

# Add remotes manually
git remote add origin git@github.com:username/repo.git

# Push
git push -u origin main --tags
```

## Best Practices

1. **Test First**: Run with `--no-push --no-timestamp` to test locally
2. **Verify Immediately**: Check GPG signature right after claiming
3. **Monitor Timestamps**: Use monitor script to track OTS confirmation
4. **Backup Keys**: Your GPG key is critical - back it up securely
5. **Document**: Keep notes on what files you've claimed

## Next Steps

- Read [ASSUMPTIONS.md](../../ASSUMPTIONS.md) to understand implementation details
- Configure Git remotes for your repositories
- Set up automated monitoring
- Integrate into your workflow/CI pipeline

## Advanced Usage

### Custom Configuration

Create `claim-forge.toml`:

```toml
[claim-forge]
default_license = "MIT"
gitlab_enabled = false
github_enabled = true
ots_enabled = true
auto_sign = true
```

Use it:

```bash
./bin/claim-forge --config claim-forge.toml --interactive
```

### Batch Processing

Create a script to claim multiple files:

```bash
#!/bin/bash
for file in papers/*.pdf; do
  ./bin/claim-forge \
    --repo research-papers \
    --file "$file" \
    --description "Research paper: $(basename $file)" \
    --license "Palimpsest"
done
```

### Integration with CI/CD

Add to `.gitlab-ci.yml`:

```yaml
claim:
  stage: deploy
  script:
    - ./bin/claim-forge --repo $CI_PROJECT_NAME --file artifact.zip --description "Release $CI_COMMIT_TAG" --no-push
    - git push origin main --tags
  only:
    - tags
```
