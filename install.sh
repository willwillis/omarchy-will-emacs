#!/bin/bash

# will-omarchy-emacs installer
# Installs and configures Emacs for omarchy users

set -eE

echo "Installing omarchy-emacs..."

# Install Emacs via pacman
echo "Installing Emacs..."
sudo pacman -S --noconfirm --needed emacs

# Install essential packages for Emacs ecosystem
echo "Installing Emacs ecosystem packages..."
sudo pacman -S --noconfirm --needed \
  ripgrep \
  fd \
  git

# Create Emacs config directory
mkdir -p ~/.config/emacs

# Install our configuration files
echo "Installing Will's omarchy-emacs configuration..."
cp -r config/* ~/.config/emacs/

# Install will-emacs launcher
echo "Installing will-emacs launcher..."
mkdir -p ~/.local/bin
cp bin/will-emacs ~/.local/bin/
chmod +x ~/.local/bin/will-emacs

# Install desktop file for rofi/application launcher
echo "Installing desktop file..."
mkdir -p ~/.local/share/applications
cp applications/will-emacs.desktop ~/.local/share/applications/

# Pre-install Emacs packages to avoid first-run delay
echo "Pre-installing Emacs packages (this may take a moment)..."
emacs --batch --init-directory="$HOME/.config/emacs" --eval "
(progn
  (require 'package)
  (package-initialize)
  (package-refresh-contents)
  ;; Install all use-package dependencies
  (dolist (pkg '(ef-themes which-key ivy counsel magit projectile counsel-projectile indent-bars))
    (unless (package-installed-p pkg)
      (package-install pkg)))
  (message \"Package installation complete\"))" 2>/dev/null || true

echo "will-omarchy-emacs installation complete!"
echo ""
echo "To start using Will's omarchy-emacs:"
echo "1. Launch: will-emacs"
echo "2. The configuration will automatically:"
echo "   - Sync with your current omarchy theme"
echo "   - Install required packages on first run"
echo "   - Set up optimized defaults"
echo ""
echo "Note: Regular 'emacs' command remains unchanged for vanilla emacs"
