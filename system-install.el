;;; system-install.el --- Wrappers for package managers-*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021-2022 Andrew Peck

;; Author: Andrew Peck <andrew.peck@cern.ch>
;; URL: https://github.com/andrewpeck/system-install.el
;; Version: 0.0.0
;; Package-Requires: ((s))
;; Keywords: tools vhdl fpga
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>

;;; Commentary:

;;; Code:

(require 's)

(defvar system-install-package-cache-file "~/.emacs.d/system-package-cache.json")
(defvar system-install-package-cache-refresh-days 7)

;; trim the 2 header lines off of the package list output, and remove duplicate lines
(defvar system-install--dnf-filter-cmd
  "awk -F. 'NF > 2 {this = $1; if (this != prev) {print this}; prev = this}'")

(defvar system-install-exe
  (cond ((executable-find "dnf")    'dnf)
        ((executable-find "pacman") 'pacman)
        ((executable-find "apt")    'apt)))

(defun system-install-get-package-cmd ()
  (pcase system-install-exe
    ('dnf    "dnf")
    ('pacman "pacman")
    ('apt    "apt")))

(defun system-install-get-package-info-flag ()
  (pcase system-install-exe
    ('dnf    "info")
    ('pacman "-Si")
    ('apt    "show")))

(defun system-install-get-package-install-flag ()
  (pcase system-install-exe
    ('dnf    "install")
    ('pacman "-S")
    ('apt    "install")))

(defun system-install-get-package-update-flag ()
  (pcase system-install-exe
    ('dnf    "update")
    ('pacman "-Sy")
    ('apt    "install")))

(defun system-install-get-package-remove-flag ()
  (pcase system-install-exe
    ('dnf    "remove")
    ('pacman "-R")
    ('apt    "uninstall")))

(defun system-install-get-system-upgrade-flag ()
  (pcase system-install-exe
    ('dnf    "update")
    ('pacman "-Syu")
    ('apt    "upgrade")))

(defun system-install-get-package-list-cmd ()
  (pcase system-install-exe
    ('dnf    (concat  "dnf -C list available | " system-install--dnf-filter-cmd))
    ('pacman "pacman -Sl | awk '{print $2}'")
    ('apt    "apt-cache search . | awk '{print $1}'")))

(defun system-install-get-installed-package-list-cmd ()
  (pcase system-install-exe
    ('dnf (concat  "dnf -C list installed | " system-install--dnf-filter-cmd))
    ('pacman "pacman -Q | awk '{print $2}'")
    ('apt "apt list --installed 2> /dev/null | awk -F\/ '/\[installed/ {print $1}'")))

(defun system-install-get-clean-cache-cmd ()
  (pcase system-install-exe
    ('pacman "pacman -Sc")
    ('apt "apt-get clean")
    ('dnf "dnf clean all")))

;;;###autoload
(defun system-install-clean-cache ()
  "Clean system package cache"
  (interactive)
  (with-editor-async-shell-command (system-install-get-clean-cache-cmd)))

;; generic functions

(defun system-install-get-package-list ()
  ;; if we have no cache, or it is out of date generate one
  (if (or  (not (file-exists-p system-install-package-cache-file))
           (> (time-to-seconds
               (subtract-time (current-time)
                              (file-attribute-modification-time
                               (file-attributes system-install-package-cache-file ))))
              (* 60 60 24 system-install-package-cache-refresh-days)))
      (let ((package-list (s-split "\n" (shell-command-to-string (system-install-get-package-list-cmd)) t)))
        (with-temp-file system-install-package-cache-file
          (insert (json-encode package-list)))
        package-list)

    ;; if it exists and is up to date, just return the cache
    (let ((json-array-type 'list))
      (json-read-file system-install-package-cache-file))))

(defun system-install-get-installed-package-list ()
  (s-split "\n"
           (shell-command-to-string
            (system-install-get-installed-package-list-cmd)) t))

(define-minor-mode system-install-run-minor-mode
  "Minor mode for buffers running brew commands"
  :keymap '(("q" .  bury-buffer)))

(with-eval-after-load 'evil
  (evil-define-key 'normal system-install-run-minor-mode-map "q" #'bury-buffer))

(cl-defun system-install-run (subcmd &key args noroot)
  (let* ((name (format "%s" subcmd))
         (buf (format "*%s*" name)))

    (with-editor-async-shell-command
     (concat
      (if noroot "" "sudo ")
      (system-install-get-package-cmd) " "
      subcmd " "
      (when args (if (listp args) (string-join args " ") args)))
     buf)

    (with-current-buffer buf
      (ansi-color-apply-on-region (point-min) (point-max))
      (system-install-run-minor-mode))))

;;;###autoload
(defun system-install (package)
  "Install `package' via system installer"
  (interactive
   (list (completing-read "Formula: " (system-install-get-package-list) nil t)))
  (system-install-run (system-install-get-package-install-flag) :args package))

;;;###autoload
(defun system-upgrade-package (package)
  "Upgrade `package' to the latest version"
  (interactive
   (list (completing-read "Formula: " (system-install-get-installed-package-list) nil t)))
  (system-install-run (system-install-get-package-update-flag) :args package))

;;;###autoload
(defun system-remove-package (package)
  "Remove `package' using system package manager"
  (interactive
   (list (completing-read "Formula: " (system-install-get-installed-package-list) nil t)))
  (system-install-run (system-install-get-package-remove-flag) :args package))

;;;###autoload
(defun system-upgrade ()
  "Upgrade all system packages"
  (interactive)
  (system-install-run (system-install-get-system-upgrade-flag)))

;;;###autoload
(defun system-update ()
  "Update the package database"
  (interactive)
  (system-install-run (system-install-get-package-update-flag)))

;;;###autoload
(defun system-package-info (package)
  "Display `info' output for `package'"
  (interactive
   (list (completing-read "Formula: " (system-install-get-package-list) nil t)))
  (system-install-run (system-install-get-package-info-flag) :args package :noroot t))

(provide 'system-install)
;;; system-install.el ends here
