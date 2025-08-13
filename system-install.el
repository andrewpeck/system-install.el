;;; system-install.el --- Wrappers for package managers-*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021-2025 Andrew Peck

;; Author: Andrew Peck <andrew.peck@cern.ch>
;; URL: https://github.com/andrewpeck/system-install.el
;; Version: 0.0.0
;; Package-Requires: ((ansi-color "3.4.2") (json "1.5") (s "1.13") (with-editor "3.2.0") (emacs "28.1"))
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
;;
;; This provides some simple wrappers for installing packages on a few linux systems..
;;  not really meant for public consumption but it has been very useful for my purposes

;;; Code:

(require 'json)
(require 's)
(require 'cl-lib)
(require 'marginalia)

(defvar system-install--package-cache-file
  (concat user-emacs-directory "system-package-cache.json"))

(defvar system-install--package-description-cache-file
  (concat user-emacs-directory "system-package-description-cache.json")
  "File to store package descriptions.
Due to lookup time package descriptions are stored in a hash-map which is serialized into JSON for quick recovery.")

(defvar system-install--cache-refresh-days 7)

;; trim the 2 header lines off of the package list output, and remove duplicate lines
(defvar system-install--dnf-filter-cmd
  "awk -F. 'NF > 2 {this = $1; if (this != prev) {print this}; prev = this}'")

(defvar system-install--exe
  (cond ((executable-find "dnf")    'dnf)
        ((executable-find "pacman") 'pacman)
        ((executable-find "apt")    'apt)
        ((executable-find "zypper") 'zypper)))

(defun system-install--get-package-cmd ()
  (symbol-name system-install--exe))

(defun system-install--not-implemented-error ()
  (error (format "%s not implemented in %s"
                 (symbol-name system-install--exe)
                 (nth 1 (backtrace-frame 3)))))

(defun system-install--get-package-info-flag ()
  (pcase system-install--exe
    ('dnf    "info")
    ('pacman "-Si")
    ('apt    "show")
    (_ (system-install--not-implemented-error))))

(defun system-install--get-package-install-flag ()
  (pcase system-install--exe
    ('dnf    "install")
    ('pacman "-S")
    ('apt    "install")
    ('zypper "in")
    (_ (system-install--not-implemented-error))))

(defun system-install--get-package-update-flag ()
  (pcase system-install--exe
    ('dnf    "update")
    ('pacman "-Sy")
    ('apt    "install")
    (_ (system-install--not-implemented-error))))

(defun system-install--get-package-remove-flag ()
  (pcase system-install--exe
    ('dnf    "remove")
    ('pacman "-R")
    ('apt    "uninstall")
    ('zypper "remove")
    (_ (system-install--not-implemented-error))))

(defun system-install--get-system-upgrade-flag ()
  (pcase system-install--exe
    ('dnf    "update")
    ('pacman "-Syu")
    ('apt    "upgrade")
    ('zypper "dup")
    (_ (system-install--not-implemented-error))))

(defun system-install--get-package-list-cmd ()
  (pcase system-install--exe
    ('dnf    (concat  "dnf -C list available | " system-install--dnf-filter-cmd))
    ('pacman "pacman -Sl | awk '{print $2}'")
    ('apt    "apt-cache search . | awk '{print $1}'")
    ('zypper "zypper se | awk -F'|' '{print $2}' | tail -n +6")
    (_ (system-install--not-implemented-error))))

(defun system-install--get-installed-package-list-cmd ()
  (pcase system-install--exe
    ('dnf (concat  "dnf -C list installed | " system-install--dnf-filter-cmd))
    ('pacman "pacman -Q | awk '{print $2}'")
    ('zypper "zypper se | awk -F'|' '/^i/{print $2}' | tail -n +6")
    ('apt "apt list --installed 2> /dev/null | awk -F/ '{print $1}'")
    (_ (system-install--not-implemented-error))))

(defun system-install--get-package-description (cand)
  (gethash cand (system-install--get-cached-package-descriptions)))

(defun system-install--get-clean-cache-cmd ()
  (pcase system-install--exe
    ('pacman "pacman -Sc")
    ('apt "apt-get clean")
    ('dnf "dnf clean all")
    (_ (system-install--not-implemented-error))))

;;;###autoload
(defun system-install-clean-cache ()
  "Clean system package cache"
  (interactive)
  (async-shell-command (system-install--get-clean-cache-cmd)))

;; generic functions

(defun system-install--get-package-list ()
  ;; if we have no cache, or it is out of date generate one
  (if (or  (not (file-exists-p system-install--package-cache-file))
           (> (time-to-seconds
               (time-subtract (current-time)
                              (file-attribute-modification-time
                               (file-attributes system-install--package-cache-file ))))
              (* 60 60 24 system-install--cache-refresh-days)))
      ;; (system-install-update)
      (let ((package-list (mapcar 'string-trim (s-split "\n" (shell-command-to-string (system-install--get-package-list-cmd)) t))))
        (with-temp-file system-install--package-cache-file
          (insert (json-encode package-list)))
        package-list)

    ;; if it exists and is up to date, just return the cache
    (let ((json-array-type 'list))
      (json-read-file system-install--package-cache-file))))

(defun system-install--get-package-description-hashtable ()
  (let ((ht (make-hash-table))
        (pkgs (pcase system-install--exe
                ('zypper
                 (mapcar (lambda (x)
                           (mapcar #'string-trim (split-string x "::::")))
                         (split-string (shell-command-to-string "zypper se | tail -n +6 | awk -F'|' '{printf(\"%s::::%s\\n\", $2, $3)}'") "\n")))
                (_ (system-install--not-implemented-error)))))

    (dolist (pkg pkgs)
      (puthash (intern (car pkg)) (cadr pkg) ht)) ht))

(defvar system-install--package-description-cache nil)

(defun system-install--get-cached-package-descriptions ()
  ;; FIXME: combine shared code with the get-package-list version
  ;; if we have no cache, or it is out of date generate one
  (when (or  (not (file-exists-p system-install--package-description-cache-file))
           (> (time-to-seconds
               (time-subtract (current-time)
                              (file-attribute-modification-time
                               (file-attributes system-install--package-description-cache-file ))))
              (* 60 60 24 system-install--cache-refresh-days)))

      (with-temp-file system-install--package-description-cache-file
        (insert (json-encode (system-install--get-package-description-hashtable)))))

  ;; if it exists and is up to date, just return the cache
  (unless system-install--package-description-cache
    (let ((json-array-type 'list)
          (json-object-type 'hash-table))
      (setq system-install--package-description-cache
            (json-read-file system-install--package-description-cache-file))))
  system-install--package-description-cache)

(setq system-install--package-description-cache nil)

(defun system-install--get-installed-package-list ()
  (s-split "\n"
           (shell-command-to-string
            (system-install--get-installed-package-list-cmd)) t))

(define-minor-mode system-install--run-minor-mode
  "Minor mode for buffers running system install commands"
  :keymap '(("q" .  bury-buffer)))

(cl-defun system-install--run (subcmd &key args noroot)
  (let* ((name (format "%s" subcmd))
         (buf (format "*%s*" name)))

    (async-shell-command
     (concat
      (if noroot "" "sudo ")
      (system-install--get-package-cmd) " "
      subcmd " "
      (when args (if (listp args) (string-join args " ") args)))
     buf)

    (with-current-buffer buf
      (ansi-color-apply-on-region (point-min) (point-max))
      (system-install--run-minor-mode))))

;;;###autoload
(defun system-install (package)
  "Install `package' via system installer"
  (interactive
   (list (completing-read "Formula: " (system-install--get-package-list) nil t)))
  (system-install--run (system-install--get-package-install-flag) :args package))

;;;###autoload
(defun system-install-upgrade-package (package)
  "Upgrade `package' to the latest version"
  (interactive
   (list (completing-read "Formula: " (system-install--get-installed-package-list) nil t)))
  (system-install--run (system-install--get-package-update-flag) :args package))

;;;###autoload
(defun system-install-remove-package (package)
  "Remove `package' using system package manager"
  (interactive
   (list (completing-read "Formula: " (system-install--get-installed-package-list) nil t)))
  (system-install--run (system-install--get-package-remove-flag) :args package))

;;;###autoload
(defun system-install-upgrade ()
  "Upgrade all system packages"
  (interactive)
  (system-install--run (system-install--get-system-upgrade-flag)))

;;;###autoload
(defun system-install-update ()
  "Update the package database"
  (interactive)
  (system-install--run (system-install--get-package-update-flag)))

;;;###autoload
(defun system-install-package-info (package)
  "Display `info' output for `package'"
  (interactive
   (list (completing-read "Formula: " (system-install--get-package-list) nil t)))
  (system-install--run (system-install--get-package-info-flag) :args package :noroot t))

(defun system-install--annotator-function (cand)
  "Marginalia annotator for system-install."
  (marginalia--fields
   ((system-install--get-package-description cand))))

(add-to-list 'marginalia-annotators '(system-install-category system-install--annotator-function none))
(add-to-list 'marginalia-command-categories '(system-install . system-install-category))
(add-to-list 'marginalia-command-categories '(system-install-remove-package . system-install-category))
(add-to-list 'marginalia-command-categories '(system-install-upgrade-package . system-install-category))

(provide 'system-install)
;;; system-install.el ends here
