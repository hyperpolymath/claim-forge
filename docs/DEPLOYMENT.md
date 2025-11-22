# Claim Forge - Deployment Guide

This guide covers deploying Claim Forge in various environments.

## Table of Contents

- [Local Development](#local-development)
- [System-Wide Installation](#system-wide-installation)
- [Container Deployment](#container-deployment)
- [SaltStack Automation](#saltstack-automation)
- [CI/CD Integration](#cicd-integration)
- [Production Deployment](#production-deployment)

---

## Local Development

### Quick Setup

```bash
git clone https://github.com/Hyperpolymath/claim-forge.git
cd claim-forge
./setup.sh
```

The setup script will:
1. Detect your OS
2. Install dependencies (GNAT, Git, GPG, Python, OTS)
3. Configure Git
4. Check GPG keys
5. Build the project

### Manual Setup

If the setup script doesn't work for your OS:

```bash
# Install dependencies (example for Fedora)
sudo dnf install gcc-gnat gprbuild make git gnupg2 python3 python3-pip

# Install OpenTimestamps
pip3 install --user opentimestamps-client

# Configure Git
git config --global user.name "Your Name"
git config --global user.email "your@email.com"

# Create/configure GPG key
gpg --full-generate-key
gpg --list-secret-keys
git config --global user.signingkey YOUR_KEY_ID

# Build
make clean && make release
```

### Testing Local Build

```bash
# Run tests
./docs/examples/test-workflow.sh

# Run help
./bin/claim-forge --help

# Run interactive mode
./bin/claim-forge
```

---

## System-Wide Installation

### Install to /usr/local

```bash
sudo make install PREFIX=/usr/local
```

This installs:
- Binary: `/usr/local/bin/claim-forge`
- Data: `/usr/local/share/claim-forge/`

### Install to Custom Location

```bash
sudo make install PREFIX=/opt/claim-forge
```

### Uninstall

```bash
sudo make uninstall PREFIX=/usr/local
```

### Post-Installation

```bash
# Verify installation
which claim-forge
claim-forge --version

# Test
claim-forge --help
```

---

## Container Deployment

### Using Podman (Recommended)

```bash
# Build image
podman build -t claim-forge:latest .

# Run interactive
podman run -it --rm \
  -v $(pwd):/workspace \
  -v ~/.gnupg:/root/.gnupg:ro \
  claim-forge:latest

# Run with custom command
podman run -it --rm \
  -v $(pwd):/workspace \
  claim-forge:latest \
  --repo my-project --file document.pdf --description "My claim"
```

### Using Docker

```bash
# Build image
docker build -t claim-forge:latest .

# Run interactive
docker run -it --rm \
  -v $(pwd):/workspace \
  -v ~/.gnupg:/root/.gnupg:ro \
  claim-forge:latest

# Run shell for debugging
docker run -it --rm \
  -v $(pwd):/workspace \
  claim-forge:latest \
  /bin/bash
```

### Container Configuration

For GPG signing in containers:

```bash
# Option 1: Mount GPG directory (less secure)
-v ~/.gnupg:/root/.gnupg

# Option 2: Use gpg-agent forwarding (more secure)
# On host: export GPG_AGENT_INFO
# In container: mount GPG_AGENT_INFO socket
```

For Git credentials:

```bash
# Mount SSH keys
-v ~/.ssh:/root/.ssh:ro

# Or use Git credential helper
docker run -e GIT_CREDENTIALS=... claim-forge
```

### Building Multi-Arch Images

```bash
# Build for multiple architectures
podman build --platform linux/amd64,linux/arm64 -t claim-forge:latest .

# Push to registry
podman tag claim-forge:latest registry.example.com/claim-forge:latest
podman push registry.example.com/claim-forge:latest
```

---

## SaltStack Automation

### Prerequisites

```bash
# Install SaltStack
sudo dnf install salt-master salt-minion  # Fedora/RHEL
sudo apt install salt-master salt-minion  # Debian/Ubuntu
```

### Deploy State File

```bash
# Copy state file to Salt file server
sudo cp salt/claim-forge.sls /srv/salt/

# Apply state
sudo salt '*' state.apply claim-forge

# Test mode (dry run)
sudo salt '*' state.apply claim-forge test=True
```

### Customizing Deployment

Edit `salt/claim-forge.sls`:

```yaml
# Custom installation path
claim-forge-directory:
  file.directory:
    - name: /opt/custom-path/claim-forge

# Custom Git configuration
git-config-email:
  git.config_set:
    - name: user.email
    - value: "custom@example.com"
```

### Multi-Minion Deployment

```bash
# Deploy to specific minions
sudo salt 'web*' state.apply claim-forge

# Deploy to all production servers
sudo salt -G 'environment:production' state.apply claim-forge
```

---

## CI/CD Integration

### GitLab CI/CD

The project includes `.gitlab-ci.yml` with stages:

1. **Build**: Compile Ada application
2. **Test**: Run test suite
3. **Package**: Create container image
4. **Deploy**: Deploy to production

#### Configuration

```yaml
# .gitlab-ci.yml variables
variables:
  BUILD_MODE: release
  CONTAINER_REGISTRY: registry.gitlab.com/username/claim-forge
```

#### Pipeline Execution

```bash
# Trigger pipeline
git push origin main

# Create release
git tag -s v1.0.0 -m "Release v1.0.0"
git push origin v1.0.0
```

### GitHub Actions

Create `.github/workflows/build.yml`:

```yaml
name: Build and Test

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2
    - name: Install dependencies
      run: |
        sudo apt-get update
        sudo apt-get install -y gnat gprbuild
    - name: Build
      run: make release
    - name: Test
      run: ./docs/examples/test-workflow.sh
```

### Jenkins

```groovy
pipeline {
    agent any
    stages {
        stage('Build') {
            steps {
                sh 'make clean && make release'
            }
        }
        stage('Test') {
            steps {
                sh './docs/examples/test-workflow.sh'
            }
        }
        stage('Deploy') {
            steps {
                sh 'sudo make install PREFIX=/opt/claim-forge'
            }
        }
    }
}
```

---

## Production Deployment

### Server Requirements

- **OS**: Linux (Fedora, RHEL, Ubuntu, Debian)
- **CPU**: 1+ cores
- **RAM**: 256MB+
- **Disk**: 100MB+ for application, variable for claims
- **Network**: Outbound HTTPS for OpenTimestamps

### Security Considerations

1. **GPG Key Management**
   - Use hardware security keys for production
   - Separate signing keys per environment
   - Regular key rotation policy

2. **Access Control**
   - Restrict binary execution to authorized users
   - Use file permissions (chmod 750)
   - SELinux/AppArmor policies

3. **Logging & Monitoring**
   - Central log aggregation
   - Monitor for failed signature attempts
   - Alert on OTS verification failures

4. **Network Security**
   - Firewall rules for Git push
   - TLS for all remote operations
   - VPN for sensitive deployments

### Production Checklist

- [ ] GPG keys configured and backed up
- [ ] Git remotes configured with SSH keys
- [ ] OpenTimestamps client installed and tested
- [ ] Monitoring script deployed
- [ ] Log rotation configured
- [ ] Backup strategy for claims and signatures
- [ ] Disaster recovery plan documented
- [ ] Security audit completed
- [ ] User training completed

### Monitoring Setup

```bash
# Install monitoring script
sudo cp bin/monitor.sh /usr/local/bin/claim-forge-monitor
sudo chmod +x /usr/local/bin/claim-forge-monitor

# Create systemd timer for periodic checks
sudo cat > /etc/systemd/system/claim-forge-monitor.timer <<EOF
[Unit]
Description=Claim Forge Health Monitor Timer

[Timer]
OnCalendar=hourly
Persistent=true

[Install]
WantedBy=timers.target
EOF

# Create service
sudo cat > /etc/systemd/system/claim-forge-monitor.service <<EOF
[Unit]
Description=Claim Forge Health Monitor

[Service]
Type=oneshot
ExecStart=/usr/local/bin/claim-forge-monitor health
StandardOutput=journal
EOF

# Enable and start
sudo systemctl enable claim-forge-monitor.timer
sudo systemctl start claim-forge-monitor.timer
```

### Backup Strategy

```bash
# Backup GPG keys (secure storage!)
gpg --export-secret-keys > gpg-secret-backup.asc
gpg --export-ownertrust > gpg-ownertrust-backup.txt

# Backup claims and signatures
tar -czf claims-backup.tar.gz CLAIM.md* *.ots *.sig

# Backup Git repository
git bundle create repo-backup.bundle --all
```

### Upgrading

```bash
# 1. Backup current installation
sudo cp /usr/local/bin/claim-forge /usr/local/bin/claim-forge.old

# 2. Pull updates
git pull origin main

# 3. Rebuild
make clean && make release

# 4. Test in staging
./docs/examples/test-workflow.sh

# 5. Install
sudo make install PREFIX=/usr/local

# 6. Verify
claim-forge --version

# 7. Rollback if needed
# sudo mv /usr/local/bin/claim-forge.old /usr/local/bin/claim-forge
```

---

## Troubleshooting

### Common Issues

#### Build Fails

```bash
# Check GNAT version
gnat --version  # Should be 2021+

# Clean and rebuild
make distclean && make release

# Check for missing dependencies
./setup.sh
```

#### GPG Signing Fails

```bash
# Check GPG keys
gpg --list-secret-keys

# Test GPG
echo "test" | gpg --clearsign

# Configure Git
git config --global commit.gpgsign true
git config --global user.signingkey YOUR_KEY_ID
```

#### OpenTimestamps Fails

```bash
# Reinstall client
pip3 install --upgrade opentimestamps-client

# Test manually
ots stamp test.txt
ots verify test.txt.ots
```

#### Container Issues

```bash
# Check SELinux labels (Fedora/RHEL)
ls -Z $(pwd)

# Add :z or :Z to volumes
podman run -v $(pwd):/workspace:z claim-forge

# Check permissions
podman run -it claim-forge ls -la /workspace
```

---

## Support

- **Issues**: https://github.com/Hyperpolymath/claim-forge/issues
- **Documentation**: See [CLAUDE.md](../CLAUDE.md)
- **Examples**: See [examples/](examples/)

---

*Last Updated: 2025-11-22*
