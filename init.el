;;; init.el --- Emacs Initialization -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Author: Alejandro Polanco <apolanco.sosa@gmail.com>
;; URL:    <https://github.com/AlejandroPolanco/emacs.d>
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

(require 'org)

;; Load main configuration.
(if (file-exists-p (expand-file-name "configuration.el" user-emacs-directory))
     (load-file (expand-file-name "configuration.el" user-emacs-directory))
   (org-babel-load-file (expand-file-name (concat user-emacs-directory "literate_configuration/configuration.org"))))

;;; init.el ends here
