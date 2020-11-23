;;; init.el --- Emacs Initialization -*- lexical-binding: t; -*-
;;
;; Author: Alejandro Polanco <apolanco.sosa@gmail.com>
;; URL:    <https://github.com/AlejandroPolanco/emacs.d>
;;
;;; Commentary:
;; A basic monolithic configuration.
;;
;;; This file is not part of GNU Emacs.
;;
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
;;
;;; Code:

;; =============================================================================
;; STARTUP OPTIMIZATION
;; =============================================================================
;; Source: <https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org>
;; From: How does Doom start up so quickly?

(defvar user/gc-cons-threshold 16777216     ; 16 mb
  "The default value to use for `gc-cons-threshold'. If experience freezing, 
decrease this, if experience stuttering, increase this.")

;; Avoid garbage collection at startup by increasing the value
;; of `gc-cons-threshold' to defer it.
(setq gc-cons-threshold most-positive-fixnum)   ; 2^61 bytes
(setq gc-cons-percentage 0.6)

;; Reset the garbage collector after init.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold user/gc-cons-threshold)
            (setq gc-cons-percentage 0.1)))

;; =============================================================================
;; DIRECTORY MANAGEMENT
;; =============================================================================

(defconst user-data-dir
  (eval-when-compile (concat (file-truename user-emacs-directory) ".cache/"))
  "Provide a location where Emacs can store static and dynamic data.")

;; Automatically create missing directories.
(dolist (dir (list user-data-dir))
  (unless (file-directory-p dir)
    (make-directory dir 'parents)))

;; =============================================================================
;; CUSTOM
;; =============================================================================

;; Inhibit the `custom-set-variables' block in the init file.
(setq custom-file (concat user-data-dir "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file t t))

;; =============================================================================
;; LIBRARIES
;; =============================================================================

;; Built-in utility libraries that add a wealth of common-lisp inspired
;; functions and macros. These libraries will be require by other packages.
(require 'cl-lib)
(require 'subr-x)

;; =============================================================================
;; PACKAGE MANAGEMENT
;; =============================================================================

;; Emacs's built-in package management.
(require 'package)

;; Loads whichever version of the file is newest.
(setq load-prefer-newer t)

;; Package initialization occurs before user-init-file is loaded,
;; but after early-init-file.
(setq package-enable-at-startup nil)

;; Don't add that custom-set-variables block to init.
(advice-add #'package--ensure-init-file :override #'ignore)

;; Tell package.el where to store Emacs Lisp code. 
(setq package-user-dir (concat user-data-dir "elpa/"))

;; Adding a list of repositories. 
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Source: <https://github.com/jwiegley/use-package>
;; The `use-package' macro allows to isolate package configuration
;; in the .emacs files in a way that is both performance-oriented and tidy.
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
;; FRAME AND BUFFER
;; =============================================================================

;; A simple frame title.
(setq frame-title-format '("Emacs"))

;; Update UI less frequently.
(setq idle-update-delay 1.0)
(setq jit-lock-defer-time 0)

;; Give each frame/window the same number of pixels.
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)

;; The least specialized major mode.
(setq initial-major-mode 'fundamental-mode)

;; Minimal startup screen/message.
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

;; =============================================================================
;; FONT
;; =============================================================================

;; UTF-8 Coding System.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; System default font.
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 120
                    :weight 'normal
                    :width  'normal)

;; inhibit frames from resizing when the fonts are larger (or smaller)
;; that the system default. 
(setq frame-inhibit-implied-resize t)

;; Don’t compact font caches during garbage collection.
(setq inhibit-compacting-font-caches t)

;; =============================================================================
;; MINIBUFFER
;; =============================================================================

;; Show Keystrokes in Progress Instantly.
(setq echo-keystrokes 0.02)

;; Enable recursive minibuffers.
(setq enable-recursive-minibuffers t)

;; keep the point out of the minibuffer.
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))

;; Expand the minibuffer to fit multi-line text displayed in the echo-area.
(setq max-mini-window-height 0.12)
(setq resize-mini-windows 'grow-only)

;; Use y / n instead of yes / no.
(setq confirm-kill-emacs #'y-or-n-p)
(fset #'yes-or-no-p #'y-or-n-p)

;; =============================================================================
;; CURSOR
;; =============================================================================

;; Less distracting settings.
(blink-cursor-mode -1)
(setq blink-matching-paren nil)

;; Display the current column number.
(setq column-number-mode t)

;; Don't stretch the cursor to fit wide characters.
(setq x-stretch-cursor nil)

;; Keep cursor at end of lines.
(setq-default track-eol t)
(setq-default line-move-visual nil)

;; Inhibit rendering the cursor in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; =============================================================================
;; SCROLLING
;; =============================================================================
;; Source: <https://github.com/MatthewZMD/.emacs.d#smooth-scrolling>

;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 0)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)

;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)

;; =============================================================================
;; FRINGE
;; =============================================================================

;; Make the right fringe 8 pixels wide and the left disappear.
(fringe-mode '(8 . 8))

;; Reserve the fringe for more useful information.
(setq indicate-empty-lines nil)
(setq indicate-buffer-boundaries nil)

;; =============================================================================
;; GENERAL CODING STYLE
;; =============================================================================

;; Use spaces for indentation. No hard tabs.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Indent empty string and enable TAB completion.
(setq-default tab-always-indent nil)

;; Convert between tabs and spaces (only tabify initial white-space).
(setq tabify-regexp "^\t* [ \t]+")

;; Inhibit wrapping words/lines by default.
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows nil)

;; Assume that sentences end with one space rather than two.
(setq sentence-end-double-space nil)

;; Best practice following the POSIX standard.
;; <https://stackoverflow.com/questions/729692/>
(setq require-final-newline t)

;; Make apropos more useful.
;; <https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html>
(setq apropos-do-all t)

;; =============================================================================
;; KEYBINDINGS
;; =============================================================================

;; Better super/meta keys position on Apple.
;; Source: <https://emacs.stackexchange.com/questions/26616> 
(when (eq system-type 'darwin)
  (setq mac-control-modifier nil)
  (setq mac-option-modifier  'meta)
  (setq mac-command-modifier 'control))

;; Make ESC quit prompts.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; -----------------------------------------------------------------------------
;; which-key: Emacs package that displays available keybindings in popup.
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
;; hydra: This is a package for GNU Emacs that can be used to tie related
;; commands into a family of short bindings with a common prefix - a Hydra.
;; -----------------------------------------------------------------------------
(use-package hydra
  :defer 1
  :diminish)

;; =============================================================================
;; COLORSCHEME
;; =============================================================================

;; -----------------------------------------------------------------------------
;; modus-operandi: Accessible themes for GNU Emacs, conforming with the highest
;; accessibility standard for colour contrast between background and foreground
;; values (WCAG AAA).
;; -----------------------------------------------------------------------------
(use-package modus-operandi-theme
  :demand t
  :config (load-theme 'modus-operandi t))

;; =============================================================================
;; BUILT-IN PACKAGES
;; =============================================================================

;; Keep track of last point place to resume editing in the same file.
(use-package saveplace
  :ensure nil
  :demand t
  :config
  (setq save-place-file (concat user-data-dir "saveplace"))
  (setq save-place-limit 100)
  (save-place-mode 1))

;; highlight matching delimiters.
(use-package paren
  :ensure nil
  :defer 1
  :hook (prog-mode-hook . show-paren-mode)
  :config 
  (setq show-paren-delay 0.1)
  (setq show-paren-highlight-openparen t)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

;; Keep track of recently opened files.
(use-package recentf
  :ensure nil
  :defer 1
  :config
  (setq recentf-save-file (concat user-data-dir "recentf"))
  (setq recentf-auto-cleanup 'never)
  (setq recentf-max-saved-items 300)
  (setq recentf-max-menu-items 0)
  (recentf-mode 1))

;; Revert buffers when underlying files change.
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

;; Persist history.
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

;; Display function signatures or contextual metadata.
(use-package eldoc
  :ensure nil
  :defer 5
  :diminish
  :hook (prog-mode-hook . eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.2)
  (setq eldoc-echo-area-use-multiline-p nil)
  (global-eldoc-mode 1))

;; Command for compiling files according to major-mode.
(use-package compile
  :ensure nil
  :defer 5
  :config
  (setq compilation-always-kill t)
  (setq compilation-ask-about-save nil)
  (setq compilation-scroll-output 'first-error))

;; =============================================================================
;; SHELL
;; =============================================================================

;; -----------------------------------------------------------------------------
;; exec-path-from-shell: Make Emacs use the $PATH set up by the user's shell.
;; -----------------------------------------------------------------------------
(when (eq system-type 'darwin)
  (use-package exec-path-from-shell
    :demand t
    :config
    (setq exec-path-from-shell-variables '("PATH" "PYTHONPATH"))
    (exec-path-from-shell-initialize)))

;; =============================================================================
;; DIRECTORY EDITOR
;; =============================================================================

(use-package dired
  :ensure nil
  :hook ((dired-mode-hook . auto-revert-mode)
         (dired-mode-hook . dired-hide-details-mode))
  :init
  (setq dired-dwim-target t)
  (setq dired-use-ls-dired nil)
  (setq dired-auto-revert-buffer t)
  (setq dired-hide-details-hide-symlink-targets nil)
  (setq dired-listing-switches "-alh --group-directories-first")
  ;; Always copy/delete recursively
  (setq dired-recursive-copies  'always)
  (setq dired-recursive-deletes 'top)
  :config

  (defhydra hydra-dired (:hint nil :color pink)
    "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp
T - tag prefix
"
    ("\\" dired-do-ispell)
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("=" diredp-ediff)         ;; smart diff
    ("?" dired-summary)
    ("$" diredp-hide-subdir-nomove)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy)        ;; Copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer)        ;; read all directories again (refresh)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay)   ;; relist the marked or singel directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("r" dired-do-rsynch)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ("z" diredp-compress-this-file)
    ("Z" dired-do-compress)
    ("q" nil)
    ("." nil :color blue))

  (define-key dired-mode-map "?" 'hydra-dired/body))

;; =============================================================================
;; BACKUP
;; =============================================================================

;; Don't save anything or create lock/history/backup files.
;; <https://www.emacswiki.org/emacs/BackupDirectory>
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;; =============================================================================
;; VERSION CONTROL SYSTEM
;; =============================================================================

;; Preference VCS over Emacs built-in backups tools.
(setq version-control t)
(setq vc-follow-symlinks t)
(setq delete-old-versions t)

;; Convenient UI to browse through the differences between files or buffers.
(setq ediff-diff-options "-w")  ; turn off whitespace checking.
(setq ediff-split-window-function #'split-window-horizontally)
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;; -----------------------------------------------------------------------------
;; Magit: A Git Porcelain inside Emacs. Magit is an interface to the
;; version control system Git, implemented as an Emacs package.
;; -----------------------------------------------------------------------------
(use-package magit
  :defer 1
  :init
  ;; Must be set early to prevent ~/.emacs.d/transient from being created.
  (setq transient-levels-file  (concat user-data-dir "transient/levels"))
  (setq transient-values-file  (concat user-data-dir "transient/values"))
  (setq transient-history-file (concat user-data-dir "transient/history")))

;; =============================================================================
;; FILETYPE
;; =============================================================================

;; -----------------------------------------------------------------------------
;; Clojure/ClojureScript
;; -----------------------------------------------------------------------------

;; Emacs support for the Clojure(Script) programming language.
(use-package clojure-mode
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.cljc\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)))

;; The Clojure Interactive Development Environment that Rocks for Emacs 
(use-package cider
  :after (clojure-mode)
  :config
  (setq cider-repl-history-file (concat user-data-dir "cider-repl-history"))
  (setq cider-eldoc-display-for-symbol-at-point t)
  (setq cider-repl-result-prefix ";; => "))

;; Hydras for CIDER.
(use-package cider-hydra
  :init
  ;; "C-c C-d" => cider-hydra-doc/body
  ;; "C-c C-t" => cider-hydra-test/body
  ;; "C-c M-t" => cider-hydra-test/body
  ;; "C-c M-r" => cider-hydra-repl/body
  (add-hook 'clojure-mode-hook #'cider-hydra-mode))

;; -----------------------------------------------------------------------------
;; Python
;; -----------------------------------------------------------------------------

;; Built-in major mode for Python.
(use-package python
  :config
  (setq tab-width 4)
  (setq python-indent 4)
  (setq python-indent-offset 4)
  (setq python-indent-guess-indent-offset-verbose nil)

  ;; Default to Python 3. Prefer the versioned Python binaries since
  ;; some systems stupidly make the unversioned one point at Python 2.
  (cond
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python")))

  (defun python-use-correct-flycheck-executables ()
    "Use the correct Python executables for Flycheck."
    (let ((executable python-shell-interpreter))
      (save-excursion
        (goto-char (point-min))
        (save-match-data
          (when (or (looking-at "#!/usr/bin/env \\(python[^ \n]+\\)")
                    (looking-at "#!\\([^ \n]+/python[^ \n]+\\)"))
            (setq executable (substring-no-properties (match-string 1))))))
      ;; Try to compile using the appropriate version of Python for
      ;; the file.
      (setq-local flycheck-python-pycompile-executable executable)
      ;; We might be running inside a virtualenv, in which case the
      ;; modules won't be available. But calling the executables
      ;; directly will work.
      (setq-local flycheck-python-pylint-executable "pylint")
      (setq-local flycheck-python-flake8-executable "flake8")))

  (add-hook 'python-mode-hook #'python-use-correct-flycheck-executables))

;; Use the python black package to reformat your python buffers.
(use-package blacken
  :hook (python-mode-hook . blacken-mode))

;; Python virtual environment interface for Emacs.
(use-package pyvenv
  :after (python)
  :config
  (add-hook 'python-mode-local-vars-hook #'pyvenv-track-virtualenv)
  (add-to-list 'global-mode-string
               '(pyvenv-virtual-env-name (" venv:" pyvenv-virtual-env-name " "))
               'append))

;;; init.el ends here
