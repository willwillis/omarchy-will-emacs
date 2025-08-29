# omarchy-will-emacs

omarchy-will-emacs provides an Emacs experience that integrates seamlessly with your omarchy setup. Initially, this library will focus on providing emacs themes that match omarchy's themes, and update automatically when the omarchy theme is changed. 


## Screenshot
This is what you'll be presented with after installing will-emacs. (emacs is on the right)

![will-emacs screenshot](https://i.imgur.com/8hr1WeX.png)

## Features

- 🎨 **Automatic theme synchronization** with omarchy themes
- 📦 **One-command installation** via pacman integration.
- 🛠 **Essential packages** pre-configured (magit, projectile, ivy, counsel)
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

![will-emacs theme sync](https://i.imgur.com/7cwMcIe.gif)

notice in the screen recording above emacs actually detects the theme change before omarchy. 

- **gruvbox** → ef-autumn
- **tokyo-night** → ef-night
- **catppuccin** → ef-duo-dark 
- **catppuccin-latte** → ef-frost
- **rose-pine** → ef-rosa
- **nord** → ef-maris-dark
- **kanagawa** → ef-owl
- **everforest** → ef-bio
- **matte-black** → ef-dark
- **osaka-jade** → ef-elea-dark
- **ristretto** → ef-melissa-dark

you can preview all `ef-themes` [here](https://protesilaos.com/emacs/ef-themes-pictures) and omarchy themes [here](https://learn.omacom.io/2/the-omarchy-manual/52/themes)


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

## Caveats
- I have to run `will-omarchy-start-file-watch` in order to have themes switch automatically, maybe someone can help get this working automagically. see `(add-hook 'after-init-hook #'will-omarchy-setup)` in `init.el`
- I've gone through a chosen a close ef-theme for the background, perhaps I should be looking for the closest neovim equivalent. idk.
- naming. I stink at it. help me please.
  - repo: omarchy-will-emacs
  - package: will-omarch-emacs
  - launcher: will-emacs

## Contributing

This project mirrors omarchy's approach to system configuration. Contributions should maintain the opinionated, batteries-included philosophy while keeping the configuration approachable for omarchy users.

## License

MIT License - See LICENSE file for details