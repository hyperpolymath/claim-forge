# Claim Forge - SaltStack State File
# Automated system configuration and deployment

# Install required system packages
claim-forge-packages:
  pkg.installed:
    - pkgs:
      - gcc-gnat
      - gprbuild
      - make
      - git
      - gnupg2
      - python3
      - python3-pip

# Install OpenTimestamps client
opentimestamps-client:
  pip.installed:
    - name: opentimestamps-client
    - require:
      - pkg: claim-forge-packages

# Create application directory
claim-forge-directory:
  file.directory:
    - name: /opt/claim-forge
    - user: root
    - group: root
    - mode: 755
    - makedirs: True

# Clone the repository
claim-forge-repo:
  git.latest:
    - name: https://github.com/Hyperpolymath/claim-forge.git
    - target: /opt/claim-forge
    - user: root
    - require:
      - pkg: claim-forge-packages
      - file: claim-forge-directory

# Build the application
claim-forge-build:
  cmd.run:
    - name: make clean && make release
    - cwd: /opt/claim-forge
    - require:
      - git: claim-forge-repo
    - unless: test -f /opt/claim-forge/bin/claim-forge

# Create symbolic link in /usr/local/bin
claim-forge-symlink:
  file.symlink:
    - name: /usr/local/bin/claim-forge
    - target: /opt/claim-forge/bin/claim-forge
    - require:
      - cmd: claim-forge-build

# Create workspace directory
claim-forge-workspace:
  file.directory:
    - name: /var/lib/claim-forge
    - user: root
    - group: root
    - mode: 755
    - makedirs: True

# Configure Git globally (example - customize per deployment)
git-config-name:
  git.config_set:
    - name: user.name
    - value: "Claim Forge System"
    - is_global: True
    - require:
      - pkg: claim-forge-packages

git-config-email:
  git.config_set:
    - name: user.email
    - value: "admin@example.com"
    - is_global: True
    - require:
      - pkg: claim-forge-packages

# Create systemd service (for background processing)
claim-forge-service:
  file.managed:
    - name: /etc/systemd/system/claim-forge.service
    - source: salt://claim-forge/claim-forge.service
    - user: root
    - group: root
    - mode: 644
    - template: jinja
    - defaults:
        binary_path: /opt/claim-forge/bin/claim-forge
        workspace: /var/lib/claim-forge

# Enable and start service (if needed)
claim-forge-service-running:
  service.dead:
    - name: claim-forge
    - enable: False
    - require:
      - file: claim-forge-service
    # Note: Service is disabled by default as claim-forge is CLI-driven
    # Enable if implementing background monitoring/processing

# Logrotate configuration
claim-forge-logrotate:
  file.managed:
    - name: /etc/logrotate.d/claim-forge
    - contents: |
        /var/log/claim-forge/*.log {
            daily
            rotate 30
            compress
            delaycompress
            missingok
            notifempty
            create 0640 root root
        }
    - user: root
    - group: root
    - mode: 644

# Create log directory
claim-forge-logs:
  file.directory:
    - name: /var/log/claim-forge
    - user: root
    - group: root
    - mode: 755
    - makedirs: True
