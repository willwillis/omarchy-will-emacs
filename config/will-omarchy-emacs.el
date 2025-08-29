;;; will-omarchy-emacs.el --- Will's opinionated Emacs configuration for omarchy

;; Copyright (C) 2025
;; Author: Will Willis <will@willwillis.dev>
;; Version: 1.0
;; Package-Requires: ((emacs "27") (ef-themes "1.0"))

;;; Commentary:

;; This package automatically syncs Emacs ef-themes with omarchy system themes.
;; When omarchy's theme changes, Emacs will switch to the corresponding ef-theme.

;;; Code:

(require 'ef-themes)

;; Mapping omarchy themes to ef-themes
(defcustom will-omarchy-theme-mapping
  '(("gruvbox" . ef-autumn)
    ("tokyo-night" . ef-night)
    ("catppuccin" . ef-dream)
    ("catppuccin-latte" . ef-day)
    ("rose-pine" . ef-rosa)
    ("nord" . ef-frost)
    ("kanagawa" . ef-owl)
    ("everforest" . ef-bio)
    ("matte-black" . ef-dark)
    ("osaka-jade" . ef-spring)
    ("ristretto" . ef-cherie))
  "Mapping of omarchy theme names to ef-themes."
  :type '(alist :key-type string :value-type symbol)
  :group 'will-omarchy)

(defvar will-omarchy-current-theme nil
  "Currently applied omarchy theme name.")

(defun will-omarchy-get-current-theme ()
  "Get the current omarchy theme name."
  (when (file-exists-p "~/.config/omarchy/current/theme")
    (let ((theme-path (file-truename "~/.config/omarchy/current/theme")))
      (file-name-nondirectory theme-path))))

(defun will-omarchy-apply-theme (theme-name)
  "Apply the ef-theme corresponding to THEME-NAME."
  (when-let ((ef-theme (cdr (assoc theme-name will-omarchy-theme-mapping))))
    (load-theme ef-theme t)
    (message "Applied ef-theme: %s (for omarchy theme: %s)" ef-theme theme-name)
    (setq will-omarchy-current-theme theme-name)))

(defun will-omarchy-check-and-sync ()
  "Check if omarchy theme changed and sync ef-theme accordingly."
  (let ((current-theme (will-omarchy-get-current-theme)))
    (when (and current-theme 
               (not (string= current-theme will-omarchy-current-theme)))
      (will-omarchy-apply-theme current-theme))))

(defvar will-omarchy-sync-timer nil
  "Timer for checking omarchy theme changes.")

(defun will-omarchy-start-sync ()
  "Start automatic syncing of omarchy and ef-themes."
  (interactive)
  ;; Apply current theme immediately
  (when-let ((current-theme (will-omarchy-get-current-theme)))
    (will-omarchy-apply-theme current-theme))
  
  ;; Set up timer to check for changes every 2 seconds
  (setq will-omarchy-sync-timer
        (run-with-timer 0 2 #'will-omarchy-check-and-sync))
  (message "Started omarchy-ef theme syncing"))

(defun will-omarchy-stop-sync ()
  "Stop automatic syncing of omarchy and ef-themes."
  (interactive)
  (when will-omarchy-sync-timer
    (cancel-timer will-omarchy-sync-timer)
    (setq will-omarchy-sync-timer nil)
    (message "Stopped omarchy-ef theme syncing")))

;; Alternative: file watching approach (requires filenotify)
(defvar will-omarchy-file-watch nil
  "File watcher for omarchy theme changes.")

(defun will-omarchy-start-file-watch ()
  "Start watching omarchy theme file for changes."
  (interactive)
  (require 'filenotify)
  (when (file-exists-p "~/.config/omarchy/current")
    ;; Apply current theme immediately
    (when-let ((current-theme (will-omarchy-get-current-theme)))
      (will-omarchy-apply-theme current-theme))
    
    ;; Watch the directory since symlinks can be tricky
    (setq will-omarchy-file-watch
          (file-notify-add-watch
           "~/.config/omarchy/current"
           '(change)
           (lambda (event)
             (when (string-match "theme" (file-name-nondirectory (nth 2 event)))
               (run-with-timer 0.1 nil #'will-omarchy-check-and-sync)))))
    (message "Started omarchy-ef file watching")))

(defun will-omarchy-stop-file-watch ()
  "Stop watching omarchy theme file for changes."
  (interactive)
  (when will-omarchy-file-watch
    (file-notify-rm-watch will-omarchy-file-watch)
    (setq will-omarchy-file-watch nil)
    (message "Stopped omarchy-ef file watching")))

;; Manual sync function
(defun will-omarchy-sync-now ()
  "Manually sync current omarchy theme to ef-theme."
  (interactive)
  (if-let ((current-theme (will-omarchy-get-current-theme)))
      (will-omarchy-apply-theme current-theme)
    (message "No omarchy theme detected")))

;; Setup function to add to your init.el
;;;###autoload
(defun will-omarchy-setup ()
  "Set up omarchy-ef theme syncing. Add this to your init.el."
  (interactive)
  ;; Add ef-themes to load path if needed - uncomment and modify path as needed
  ;; for custom ef-themes installations or local repositories
  ;; (when (file-directory-p "~/repos/ef-themes")
  ;;   (add-to-list 'load-path "~/repos/ef-themes"))
  
  ;; Start syncing (prefer file watching if available)
  (if (fboundp 'file-notify-add-watch)
      (will-omarchy-start-file-watch)
    (will-omarchy-start-sync)))

(provide 'will-omarchy-emacs)

;;; will-omarchy-emacs.el ends here