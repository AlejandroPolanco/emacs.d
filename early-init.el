;;; early-init.el --- Early Init File -*- lexical-binding: t; -*-

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
;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.

;;; Code:

;; =============================================================================
;; STARTUP AND GARBAGE COLLECTION OPTIMIZATION
;; =============================================================================

(defvar user/file-name-handler-alist file-name-handler-alist
  "The variable `file-name-handler-alist' is consulted on every `require', `load' and
various path/io functions, but none if these is typically necessary at startup.")

(defun user/restore-file-name-handler-alist ()
  "Restore `file-name-handler-alist' because it's needed for core functionality."
  (setq file-name-handler-alist user/file-name-handler-alist))

;; Avoid garbage collection at startup by increasing the value
;; of `gc-cons-threshold' to defer it.
(setq gc-cons-threshold most-positive-fixnum)   ; 2^61 bytes
(setq gc-cons-percentage 0.6)

;; Unset `file-name-handler-alist' temporarily.
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook #'user/restore-file-name-handler-alist)

;; =============================================================================
;; PACKAGE MANAGEMENT
;; =============================================================================

;; Package initialization occurs before `user-init-file' is loaded,
;; but after `early-init-file'.
(setq package-enable-at-startup nil)

;; Don't add that `custom-set-variables' block to init.
(advice-add #'package--ensure-init-file :override #'ignore)

;; =============================================================================
;; UI ENHANCEMENTS
;; =============================================================================

;; Prevent the glimpse of un-styled Emacs by setting these early.
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;;; early-init.el ends here
