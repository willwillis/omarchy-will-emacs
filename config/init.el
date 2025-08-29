;;; init.el --- omarchy-emacs configuration

;; Copyright (C) 2025
;; Author: omarchy-emacs
;; Version: 1.0

;;; Commentary:

;; Opinionated Emacs configuration for omarchy users.
;; Provides sane defaults, modern packages, and automatic theme syncing.

;;; Code:

;; Package management setup
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Performance optimizations
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; UI improvements
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)

;; Line numbers and column numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Font configuration
(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; Essential packages
(use-package ef-themes
  :ensure t)

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy
  :ensure t
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package magit
  :ensure t)

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/repos")
    (setq projectile-project-search-path '("~/repos")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-mode))

;; Load will-omarchy-emacs configuration
(load-file (expand-file-name "will-omarchy-emacs.el" user-emacs-directory))
(require 'will-omarchy-emacs)

;; Start theme syncing automatically
(will-omarchy-setup)

;; Custom keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Better defaults
(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

;; Backup and autosave configuration
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-saves/" t)))

;; Create directories if they don't exist
(make-directory "~/.config/emacs/backups" t)
(make-directory "~/.config/emacs/auto-saves" t)

;; Better scrolling
(setq scroll-conservatively 101)
(setq scroll-margin 3)
(setq scroll-preserve-screen-position t)

;; Show matching parentheses
(show-paren-mode 1)

;; Electric pair mode
(electric-pair-mode 1)

;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(provide 'init)

;;; init.el ends here