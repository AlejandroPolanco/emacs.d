;;; early-init.el --- Early Initialization File -*- lexical-binding: t; -*-

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

;; Startup optimization

(defvar user/gc-cons-threshold 16777216     ; 16 mb
  "Set a new default value for `gc-cons-threshold'.
If experience freezing, decrease this, if experience stuttering, increase this.")

(defvar user/file-name-handler-alist file-name-handler-alist
  "The variable `file-name-handler-alist' is consulted on every `require', `load'
and various path/io functions, but none if these is typically necessary at startup.")

;; Avoid garbage collection at startup by increasing the value of `gc-cons-threshold'.
(setq gc-cons-threshold most-positive-fixnum)   ; 2^61 bytes
(setq gc-cons-percentage 0.6)

;; Don't attempt to find/apply special file handlers to files loaded during startup.
(setq file-name-handler-alist nil)

;; Make installed packages available when Emacs starts.
(setq package-enable-at-startup nil)

;; Don't add that `custom-set-variables' block to init.
(fset #'package--ensure-init-file #'ignore)

;; UI enhancements

;; The least specialized major mode.
(setq initial-major-mode 'fundamental-mode)

;; Whether frames should be resized implicitly.
(setq frame-inhibit-implied-resize t)

;; Disable GUI components early as possible.
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;;; early-init.el ends here
