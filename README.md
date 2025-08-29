# omarchy-will-emacs

omarchy-will-emacs provides an Emacs experience that integrates seamlessly with your omarchy setup. Initially, this library will focus on providing emacs themes that match omarchy's themes, and update automatically when themes are changed. This seems to be a core feature of Omarchy.

## Features

- 🎨 **Automatic theme synchronization** with omarchy themes
- 📦 **One-command installation** via pacman integration
- ⚡ **Performance optimized** with modern defaults
- 🛠 **Essential packages** pre-configured (magit, projectile, ivy, counsel) (should we add/remove from this list?)
- 🔧 **Sane defaults** for modern development
- 📁 **Project-aware** with automatic repository detection

## Installation

```bash
git clone https://github.com/willwillis/omarchy-will-emacs.git
cd omarchy-will-emacs
./install.sh
```

This will:
1. Install Emacs and essential system packages via pacman
2. Copy configuration files to `~/.config/emacs`
3. Install the `will-emacs` launcher in `~/.local/bin`
4. Set up automatic theme synchronization with omarchy

## Usage

Launch Will's configured Emacs:
```bash
will-emacs
```

The regular `emacs` command remains unchanged, so you can still use vanilla Emacs if needed.

## Theme Synchronization

omarchy-will-emacs automatically detects your current omarchy theme and applies the corresponding ef-theme:

- **gruvbox** → ef-autumn
- **tokyo-night** → ef-night
- **catppuccin** → ef-dream
- **catppuccin-latte** → ef-day
- **rose-pine** → ef-rosa
- **nord** → ef-frost
- **kanagawa** → ef-owl
- **everforest** → ef-bio
- **matte-black** → ef-dark
- **osaka-jade** → ef-spring
- **ristretto** → ef-cherie

Themes sync automatically when you change your omarchy theme, or manually with `M-x will-omarchy-sync-now`.

## Package Philosophy

Following omarchy's opinionated approach:

- **Ivy/Counsel/Swiper** for completion framework
- **Magit** for Git integration
- **Projectile** for project management
- **Which-key** for discoverable keybindings
- **ef-themes** for beautiful, accessible themes
- **JetBrains Mono** font (matches omarchy terminal)

## Configuration Structure

```
~/.config/emacs/
├── init.el                     # Main configuration
└── omarchy-ef-theme-sync.el    # Theme synchronization
```

## Manual Theme Control

```elisp
;; Sync immediately
(omarchy-ef-sync-now)

;; Start file watching (automatic)
(omarchy-ef-start-file-watch)

;; Stop file watching
(omarchy-ef-stop-file-watch)

;; Use timer-based syncing (fallback)
(omarchy-ef-start-sync)
(omarchy-ef-stop-sync)
```

## Customization

To add your own configuration, edit `~/.config/emacs/init.el` or create additional files in `~/.config/emacs/`.

The theme mapping can be customized by modifying `omarchy-ef-theme-mapping` in the configuration.

## Requirements

- [omarchy](https://github.com/willhop/omarchy) Linux distribution
- Emacs 27+
- Git (for package management)

## Contributing

This project mirrors omarchy's approach to system configuration. Contributions should maintain the opinionated, batteries-included philosophy while keeping the configuration approachable for omarchy users.

## License

MIT License - See LICENSE file for details