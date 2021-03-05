;;; init.el --- Emacs Initialization -*- lexical-binding: t; -*-

;; Author: Alejandro Polanco <apolanco.sosa@gmail.com>
;; URL:    https://github.com/AlejandroPolanco/emacs.d

;;; This file is not part of GNU Emacs.

;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A monolithic configuration.

;;; Code:

;; =============================================================================
;; Startup and Garbage Collection
;; =============================================================================

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Restore the values of gc-cons-threshold and file-name-handler-alist after init.
            (setq gc-cons-threshold user/gc-cons-threshold)
            (setq gc-cons-percentage 0.1)
            (setq file-name-handler-alist user/file-name-handler-alist)))

;; =============================================================================
;; Directory management
;; =============================================================================

(defconst user-data-dir
  (eval-when-compile (concat (file-truename user-emacs-directory) ".cache/"))
  "Provide a location where Emacs can store static and dynamic data.")

;; Automatically create missing directories.
(dolist (dir (list user-data-dir))
  (unless (file-directory-p dir)
    (make-directory dir 'parents)))

;; =============================================================================
;; Custom
;; =============================================================================

;; Inhibit the custom-set-variables block in the init file.
(setq custom-file (concat user-data-dir "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file t t))

;; =============================================================================
;; Libraries
;; =============================================================================

;; Built-in utilities libraries that add a wealth of common-lisp inspired
;; functions and macros. These libraries will be require by other packages.
(require 'cl-lib)
(require 'subr-x)

;; =============================================================================
;; Package management
;; =============================================================================

;; -----------------------------------------------------------------------------
;; Emacs's built-in package management.
;; -----------------------------------------------------------------------------
(require 'package)

;; Loads whichever version of the file is newest.
(setq load-prefer-newer t)

;; Tell package.el where to store Emacs Lisp code.
(setq package-user-dir (concat user-data-dir "elpa/"))

;; Adding a list of repositories.
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; -----------------------------------------------------------------------------
;; Use-package
;; -----------------------------------------------------------------------------
(eval-when-compile
  ;; Ensure that `use-package' and dependencies are installed.
  (unless package--initialized (package-initialize))
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'bind-key)
    (package-install 'diminish))
  (require 'use-package)
  (require 'bind-key)
  (require 'diminish))

;; Control how use-package.el handle packages.
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-always-demand nil)
(setq use-package-hook-name-suffix nil)

;; =============================================================================
;; UI Enhancements
;; =============================================================================

;; -----------------------------------------------------------------------------
;; Frame and buffer
;; -----------------------------------------------------------------------------

;; A simple frame title.
(setq frame-title-format '("Emacs"))

;; Update UI less frequently.
(setq idle-update-delay 1.0)
(setq jit-lock-defer-time 0)

;; Give each frame/window the same number of pixels.
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)

;; Minimal startup screen/message.
(setq inhibit-default-init t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; Inhibit the "For information about GNU Emacs..." message at startup.
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Hiding Scrollbar, tool bar, and menu.
(tool-bar-mode   -1)
(scroll-bar-mode -1)
(menu-bar-mode   -1)

;; Disable UI dialog.
(setq use-dialog-box nil)
(setq show-help-function nil)

;; Disable bell (both visual and audible).
(setq ring-bell-function #'ignore)
(setq visible-bell nil)

;; -----------------------------------------------------------------------------
;; Font
;; -----------------------------------------------------------------------------

;; utf-8 coding system.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; main typeface
(set-face-attribute 'default nil
                    :family "fira code"
                    :height 120
                    :weight 'normal
                    :width  'normal)

;; proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "fira code" :height 1.0)

;; monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "fira code" :height 1.0)

;; don’t compact font caches during garbage collection.
(setq inhibit-compacting-font-caches t)

;; -----------------------------------------------------------------------------
;; Minibuffer
;; -----------------------------------------------------------------------------

;; show keystrokes in progress instantly.
(setq echo-keystrokes 0.02)

;; enable recursive minibuffers.
(setq enable-recursive-minibuffers t)

;; keep the cursor out of the minibuffer.
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))

;; expand the minibuffer to fit multi-line text displayed in the echo-area.
(setq max-mini-window-height 0.12)
(setq resize-mini-windows 'grow-only)

;; use y / n instead of yes / no.
(setq confirm-kill-emacs #'y-or-n-p)
(fset #'yes-or-no-p #'y-or-n-p)

;; -----------------------------------------------------------------------------
;; Cursor
;; -----------------------------------------------------------------------------

;; less distracting settings.
(blink-cursor-mode -1)
(setq blink-matching-paren nil)

;; display the current column number.
(setq column-number-mode t)

;; don't stretch the cursor to fit wide characters.
(setq x-stretch-cursor nil)

;; keep cursor at end of lines.
(setq-default track-eol t)
(setq-default line-move-visual nil)

;; inhibit rendering the cursor in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; -----------------------------------------------------------------------------
;; Scrolling
;; -----------------------------------------------------------------------------

;; vertical scroll
(setq scroll-step 1)
(setq scroll-margin 0)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)

;; horizontal scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)

;; -----------------------------------------------------------------------------
;; Fringe
;; -----------------------------------------------------------------------------

;; make the right fringe 8 pixels wide and the left disappear.
(fringe-mode '(8 . 8))

;; reserve the fringe for more useful information.
(setq indicate-empty-lines nil)
(setq indicate-buffer-boundaries nil)

;; -----------------------------------------------------------------------------
;; General coding style
;; -----------------------------------------------------------------------------

;; Use spaces for indentation. no hard tabs.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Hitting TAB indents the current line if point is at the left margin
;; or in the line’s indentation, otherwise it inserts a "real" TAB character.
(setq-default tab-always-indent nil)

;; Assume that sentences end with one space rather than two.
(setq sentence-end-double-space nil)

(setq-default fill-column 80)

;; Inhibit wrapping words/lines by default.
(setq-default word-wrap t)
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows nil)

;; Best practice following the posix standard.
(setq require-final-newline t)

;; Make apropos more useful.
(setq apropos-do-all t)

;; =============================================================================
;; Keybindings
;; =============================================================================

;; Better super/meta keys position on apple.
(when (eq system-type 'darwin)
  (setq mac-control-modifier nil)
  (setq mac-option-modifier  'meta)
  (setq mac-command-modifier 'control))

;; -----------------------------------------------------------------------------
;; Which-key
;; -----------------------------------------------------------------------------
(use-package which-key
  :defer 1
  :diminish
  :config
  (setq which-key-separator " → ")
  (setq which-key-min-display-lines 6)
  (setq which-key-add-column-padding 1)
  (setq which-key-sort-uppercase-first nil)
  (setq which-key-sort-order #'which-key-prefix-then-key-order)
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (which-key-mode 1))

;; -----------------------------------------------------------------------------
;; Hydra
;; -----------------------------------------------------------------------------
(use-package hydra
  :defer 1
  :diminish)

;; =============================================================================
;; Built-in packages
;; =============================================================================

;; -----------------------------------------------------------------------------
;; Saveplace
;; -----------------------------------------------------------------------------
(use-package saveplace
  :ensure nil
  :demand t
  :config
  (setq save-place-file (concat user-data-dir "saveplace"))
  (setq save-place-limit 100)
  (save-place-mode 1))

;; -----------------------------------------------------------------------------
;; Paren-mode
;; -----------------------------------------------------------------------------
(use-package paren
  :ensure nil
  :defer 1
  :hook (prog-mode-hook . show-paren-mode)
  :config
  (setq show-paren-delay 0.1)
  (setq show-paren-highlight-openparen t)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

;; -----------------------------------------------------------------------------
;; Recentf-mode
;; -----------------------------------------------------------------------------
(use-package recentf
  :ensure nil
  :defer 1
  :config
  (setq recentf-save-file (concat user-data-dir "recentf"))
  (setq recentf-auto-cleanup 'never)
  (setq recentf-max-saved-items 300)
  (setq recentf-max-menu-items 0)
  (recentf-mode 1))

;; -----------------------------------------------------------------------------
;; Autorevert
;; -----------------------------------------------------------------------------
(use-package autorevert
  :ensure nil
  :defer 1
  :diminish
  :config
  (setq auto-revert-verbose t)
  (setq auto-revert-use-notify nil)
  (setq auto-revert-check-vc-info t)
  (setq revert-without-query (list "."))
  (global-auto-revert-mode 1))

;; -----------------------------------------------------------------------------
;; Savehist-mode
;; -----------------------------------------------------------------------------
(use-package savehist
  :ensure nil
  :defer 3
  :config
  (setq savehist-file (concat user-data-dir "savehist"))
  (setq savehist-save-minibuffer-history t)
  (setq savehist-autosave-interval 60)
  (setq savehist-additional-variables
        '(kill-ring                   ; persist clipboard
          search-ring                 ; persist searches
          regexp-search-ring))
  (savehist-mode 1))

;; -----------------------------------------------------------------------------
;; Eldoc-mode
;; -----------------------------------------------------------------------------
(use-package eldoc
  :ensure nil
  :defer 5
  :diminish
  :hook (prog-mode-hook . eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.2)
  (setq eldoc-echo-area-use-multiline-p nil)
  (global-eldoc-mode 1))

;; -----------------------------------------------------------------------------
;; Compile-mode
;; -----------------------------------------------------------------------------
(use-package compile
  :ensure nil
  :defer 5
  :config
  (setq compilation-always-kill t)
  (setq compilation-ask-about-save nil)
  (setq compilation-scroll-output 'first-error))

;; =============================================================================
;; Terminals & Shells
;; =============================================================================

;; -----------------------------------------------------------------------------
;; Exec-path-from-shell
;; -----------------------------------------------------------------------------
(when (eq system-type 'darwin)
  (use-package exec-path-from-shell
    :demand t
    :config
    (setq exec-path-from-shell-variables '("PATH" "INFOPATH" "PYTHONPATH"))
    (exec-path-from-shell-initialize)))

;; =============================================================================
;; Backups
;; =============================================================================

;; Don't save anything or create lock/history/backup files.
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;; =============================================================================
;; Version control system
;; =============================================================================
(setq version-control t)
(setq vc-follow-symlinks t)
(setq delete-old-versions t)

;; Convenient UI to browse through the differences between files or buffers.
(setq ediff-diff-options "-w")  ; turn off whitespace checking.
(setq ediff-split-window-function #'split-window-horizontally)
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;; -----------------------------------------------------------------------------
;; Magit
;; -----------------------------------------------------------------------------
(use-package magit
  :defer 1
  :init
  ;; Must be set early to prevent ~/.emacs.d/transient from being created.
  (setq transient-levels-file  (concat user-data-dir "transient/levels"))
  (setq transient-values-file  (concat user-data-dir "transient/values"))
  (setq transient-history-file (concat user-data-dir "transient/history")))

;; =============================================================================
;; Project management
;; =============================================================================

;; -----------------------------------------------------------------------------
;; Dired
;; -----------------------------------------------------------------------------
(use-package dired
  :ensure nil
  :hook ((dired-mode-hook . auto-revert-mode)
         (dired-mode-hook . dired-hide-details-mode))
  :config
  (setq dired-dwim-target t)
  (setq dired-use-ls-dired nil)
  (setq dired-auto-revert-buffer t)
  (setq dired-hide-details-hide-symlink-targets nil)
  (setq dired-listing-switches "-alh --group-directories-first")
  ;; Always copy/delete recursively
  (setq dired-recursive-copies  'always)
  (setq dired-recursive-deletes 'top))

;; =============================================================================
;; Programming languages
;; =============================================================================

;; -----------------------------------------------------------------------------
;; Clojure
;; -----------------------------------------------------------------------------

;; Emacs support for the Clojure(Script) programming language.
(use-package clojure-mode
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.cljc\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)))

;; CIDER: The Clojure Interactive Development Environment that Rocks for Emacs.
(use-package cider
  :after (clojure-mode)
  :config
  (setq cider-repl-history-file (concat user-data-dir "cider-repl-history"))
  (setq cider-eldoc-display-for-symbol-at-point t)
  (setq cider-repl-result-prefix ";; => "))

;; -----------------------------------------------------------------------------
;; Python
;; -----------------------------------------------------------------------------

;; Built-in major mode for Python.
(use-package python
  :config
  (setq tab-width 4)
  (setq python-indent-offset 4)
  (setq python-indent-guess-indent-offset-verbose nil)

  ;; Style used to fill docstrings.
  (setq python-fill-docstring-style 'symmetric)

  ;; Default to Python 3. Prefer the versioned Python binaries since
  ;; some systems stupidly make the unversioned one point at Python 2.
  (cond
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python")))

  ;; Automatically remove trailing whitespace when file is saved.
  (add-hook 'python-mode-hook
            (lambda()
              (add-hook 'local-write-file-hooks
                        '(lambda()
                           (save-excursion
                             (delete-trailing-whitespace)))))))

;; -----------------------------------------------------------------------------
;; Org-mode
;; -----------------------------------------------------------------------------

(defun user/org-mode-setup ()
  "Create a minimalistic user experience by disabling certain minor settings."
  (org-indent-mode)
  (setq require-final-newline nil))

(use-package org
  :hook (org-mode-hook . user/org-mode-setup)
  :init
  ;; Insead of "..." show "…" when there's hidden folded content
  ;; Some characters to choose from: …, ⤵, ▼, ↴, ⬎, ⤷, and ⋱
  (setq org-ellipsis " ⤵")
  (setq org-hide-block-startup t)
  ;; Markers
  (setq org-hide-emphasis-markers t)
  (setq org-catch-invisible-edits 'show)
  ;; List
  (setq org-list-allow-alphabetical t)
  ;; Leading stars
  (setq org-hide-leading-stars t)
  (setq org-hide-leading-stars-before-indent-mode t)
  ;; Fontify
  (setq org-return-follows-link t)
  (setq org-fontify-done-headline t)
  (setq org-fontify-quote-and-verse-blocks t)
  ;; Source code blocks
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-edit-src-content-indentation 0)
  (setq org-src-window-setup 'other-window)
  ;; Checkbox behavior
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  ;; Images
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width nil))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)))

(setq org-confirm-babel-evaluate nil)

;; https://emacs.stackexchange.com/questions/22531/
(eval-after-load 'org-indent '(diminish 'org-indent-mode))

(require 'org-tempo)
(setq org-structure-template-alist
      '(("s" . "src")
        ("q" . "quote")
        ("el" . "src emacs-lisp")
        ("clj" . "src clojure")
        ("py" . "src python")))

;; Toc-org
(use-package toc-org
  :hook (org-mode-hook . toc-org-mode))

;; -----------------------------------------------------------------------------
;; Web-mode
;; -----------------------------------------------------------------------------

(use-package web-mode
  :mode ("\\.html\\'" "\\.css\\'" "\\.js\\'")
  :bind ("C-c n" . web-mode-tag-match)
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  ;; IDE features.
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-opening t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-indentation t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-engines-alist '(("django"    . "\\.html\\'"))))

;; Emmet's support for emacs.
(use-package emmet-mode)

;;; init.el ends here
