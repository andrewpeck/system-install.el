;;; system-install.el --- Wrappers for package managers-*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021-2026 Andrew Peck

;; Author: Andrew Peck <me@andrewpeck.xyz>
;; URL: https://github.com/andrewpeck/system-install.el
;; Version: 0.0.0
;; Package-Requires: ((s "1.13") (async "1.9.8") (marginalia "1.0") (emacs "28.1"))
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
(require 'async)
(require 'marginalia)
(require 'ansi-color)
(require 'comint)
(require 'shell)

(defvar system-install--package-cache-file
  (concat user-emacs-directory "system-package-cache.json"))

(defvar system-install--package-description-cache-file
  (concat user-emacs-directory "system-package-description-cache.json")
  "File to store package descriptions.

Due to lookup time package descriptions are stored in a hash-map which
is serialized into JSON for quick recovery.")

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
  "Return the package command as a string.

Convert `system-install--exe' to its symbol name and return as a string."
  (symbol-name system-install--exe))

(defun system-install--not-implemented-error ()
  "Raise a //='not implemented//=' error for system-install.

This function signals an error indicating that a specific feature is not
implemented in the current context.  It uses the `system-install--exe' symbol
name and the function name from a backtrace frame to construct the error
message."
  (error "%s not implemented in %s"
         (symbol-name system-install--exe)
         (nth 1 (backtrace-frame 3))))

(defun system-install--get-package-info-flag ()
  "Return the package info flag for the current package manager."
  (pcase system-install--exe
    ('dnf    "info")
    ('pacman "-Si")
    ('apt    "show")
    ('zypper "info")
    (_ (system-install--not-implemented-error))))

(defun system-install--get-package-install-flag ()
  "Return the package install flag for the current package manager."
  (pcase system-install--exe
    ('dnf    "install")
    ('pacman "-S")
    ('apt    "install")
    ('zypper "in -y")
    (_ (system-install--not-implemented-error))))

(defun system-install--get-package-update-flag ()
  "Return the package update flag for the current package manager."
  (pcase system-install--exe
    ('dnf    "update")
    ('pacman "-Sy")
    ('apt    "install")
    ('zypper "update")
    (_ (system-install--not-implemented-error))))

(defun system-install--get-package-remove-flag ()
  "Return the command flag for removing a package based on the package manager."
  (pcase system-install--exe
    ('dnf    "remove")
    ('pacman "-R")
    ('apt    "remove")
    ('zypper "remove")
    (_ (system-install--not-implemented-error))))

(defun system-install--get-system-upgrade-flag ()
  "Return the system upgrade flag for the current package manager."
  (pcase system-install--exe
    ('dnf    "update")
    ('pacman "-Syu")
    ('apt    "upgrade")
    ('zypper "dup")
    (_ (system-install--not-implemented-error))))

(defun system-install--get-package-list-cmd ()
  "Return the shell command to list available packages for the current system."
  (pcase system-install--exe
    ('dnf    (concat  "dnf -C list available | " system-install--dnf-filter-cmd))
    ('pacman "pacman -Sl | awk '{print $2}'")
    ('apt    "apt-cache search . | awk '{print $1}'")
    ('zypper "zypper se | awk -F'|' '{print $2}' | tail -n +6")
    (_ (system-install--not-implemented-error))))

(defun system-install--get-installed-package-list-cmd ()
  "Return the command to list installed packages for the current system."
  (pcase system-install--exe
    ('dnf (concat  "dnf -C list installed | " system-install--dnf-filter-cmd))
    ('pacman "pacman -Q | awk '{print $2}'")
    ('zypper "zypper se | awk -F'|' '/^i/{print $2}' | tail -n +6")
    ('apt "apt list --installed 2> /dev/null | awk -F/ '{print $1}'")
    (_ (system-install--not-implemented-error))))

(defun system-install--get-package-description (cand)
  "Return package description for CAND."
  (gethash cand (system-install--get-cached-package-descriptions)))

(defun system-install--get-clean-cache-flag ()
  "Return the cache cleaning flag for the current package manager."
  (pcase system-install--exe
    ('pacman "-Sc")
    ('apt    "clean")
    ('dnf    "clean all")
    ('zypper "clean")
    (_ (system-install--not-implemented-error))))

;; generic functions

(defun system-install-refresh-cache ()
  "Refresh the cached package database."
  (interactive)

  ;; refresh the package database
  (let ((package-list (mapcar 'string-trim (s-split "\n" (shell-command-to-string (system-install--get-package-list-cmd)) t))))
    (with-temp-file system-install--package-cache-file
      (insert (json-encode package-list)))
    package-list)

  ;; refresh the package descriptions hashtable
  (with-temp-file system-install--package-description-cache-file
    (insert (json-encode (system-install--get-package-description-hashtable)))))

(defun system-install--get-package-list ()
  "Return a list of available packages, refreshing cache if necessary."
  ;; if we have no cache, or it is out of date generate one
  (if (or (not (file-exists-p system-install--package-cache-file))
          (> (time-to-seconds
              (time-subtract (current-time)
                             (file-attribute-modification-time
                              (file-attributes system-install--package-cache-file ))))
             (* 60 60 24 system-install--cache-refresh-days)))
      (system-install-refresh-cache)

    ;; if it exists and is up to date, just return the cache
    (let ((json-array-type 'list))
      (json-read-file system-install--package-cache-file))))

(defun system-install--get-package-description-hashtable ()
  "Return a hashtable of package descriptions."
  (let ((ht (make-hash-table))
        (pkgs (pcase system-install--exe
                ('zypper
                 (mapcar (lambda (x)
                           (mapcar #'string-trim (split-string x "␜")))
                         (split-string (shell-command-to-string
                                        "zypper se | tail -n +6 | awk -F'|' '{printf(\"%s␜%s\\n\", $2, $3)}'") "\n")))
                ('apt
                 (mapcar (lambda (x)
                           (mapcar #'string-trim (split-string x "␜")))
                         (split-string (shell-command-to-string "cat /var/lib/dpkg/available | grep 'Package:\|Description:' | awk '{$1= \"\"; print $0}' | paste -sd '␜\n'") "\n")))

                (_ (system-install--not-implemented-error)))))

    (dolist (pkg pkgs)
      (let ((name (car pkg))
            (desc (cadr pkg)))
        (when (and desc (not (string-empty-p name)))
          (puthash (intern name) desc ht)))) ht))

(defvar system-install--package-description-cache nil)

(defun system-install--get-cached-package-descriptions ()
  "Return cached package descriptions.

Cache is returned from `system-install--package-description-cache-file'.

If the cache does not exist or is outdated, refresh it by calling
`system-install-refresh-cache'.  Invalidate the in-memory cache to ensure
it is reloaded.  Parse the JSON file and return the cache contents as a
hash table."
  ;; FIXME: combine shared code with the get-package-list version
  ;; if we have no cache, or it is out of date generate one
  (when (or  (not (file-exists-p system-install--package-description-cache-file))
             (> (time-to-seconds
                 (time-subtract (current-time)
                                (file-attribute-modification-time
                                 (file-attributes system-install--package-description-cache-file ))))
                (* 60 60 24 system-install--cache-refresh-days)))
    ;; update json file
    (system-install-refresh-cache)
    ;; invalidate in memory cache so it will be reloaded
    (setq system-install--package-description-cache nil))

  ;; if it exists and is up to date, just return the cache
  (unless system-install--package-description-cache
    (let ((json-array-type 'list)
          (json-object-type 'hash-table))
      (setq system-install--package-description-cache
            (json-read-file system-install--package-description-cache-file))))
  system-install--package-description-cache)

(defun system-install--get-installed-package-list ()
  "Return a list of installed packages by executing the relevant shell command."
  (s-split "\n"
           (shell-command-to-string
            (system-install--get-installed-package-list-cmd)) t))

(define-minor-mode system-install--run-minor-mode
  "Minor mode for buffers running system install commands.

Only enabled once the command has exited.  While the command is still
running the buffer has to accept ordinary self-inserting input, so that
prompts such as \\='Proceed with installation? [Y/n]\\=' can be answered."
  :keymap '(("q" .  bury-buffer)))

(defvar system-install-sudo-command "sudo"
  "Command used to gain root privileges.")

(defvar system-install-sudo-prompt "[sudo] password for %u: "
  "Password prompt requested from `system-install-sudo-command'.

Passed via the SUDO_PROMPT environment variable.  Pinning the prompt to
a fixed English string keeps password detection from depending on the
locale or on which sudo implementation happens to be installed.")

(defvar system-install-password-tail-length 512
  "How many characters of recent output are searched for a password prompt.")

(defvar-local system-install--output-tail ""
  "Rolling tail of recent output in a system install buffer.

`comint-watch-for-password-prompt' inspects a single chunk of process
output at a time, and `comint-password-prompt-regexp' is anchored to the
end of that chunk.  A prompt delivered in more than one chunk therefore
goes unnoticed, and the password gets typed into the buffer in the clear.
Matching against this tail instead makes detection independent of how the
output happens to be split up.")

(defvar-local system-install--password-pending nil
  "Non-nil while a password is being read for this buffer.

Only one read may be in flight at a time.  A second one would be asked
for from inside the first one's minibuffer, which either signals an error
or sends the password at the point the process is no longer reading it.")

(defun system-install--watch-for-password (string)
  "Answer a password prompt found in STRING without echoing it.

A replacement for `comint-watch-for-password-prompt' that matches against
`system-install--output-tail' rather than STRING alone."
  (setq system-install--output-tail
        (string-limit (concat system-install--output-tail
                              (string-replace "\r" "" string))
                      system-install-password-tail-length t))
  (when (and (not system-install--password-pending)
             (let ((case-fold-search t))
               (string-match comint-password-prompt-regexp
                             system-install--output-tail)))
    (let ((prompt (concat (string-trim (match-string 0 system-install--output-tail))
                          " ")))
      ;; Consume the tail so that a re-prompt (\"Sorry, try again.\") is seen
      ;; as a new prompt rather than as a repeat of this one.
      (setq system-install--output-tail "")
      (setq system-install--password-pending t)
      ;; Read in a timer rather than inline, so the process filter is not
      ;; paused on the minibuffer.
      (run-at-time
       0 nil
       (lambda (buf prompt)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (unwind-protect
                 (when (process-live-p (get-buffer-process buf))
                   (comint-send-invisible prompt))
               ;; Cleared unconditionally, so that a retry after a rejected
               ;; password is still answered from the minibuffer.
               (setq system-install--password-pending nil)))))
       (current-buffer) prompt))))

(defvar system-install-kill-buffer-when-finished t
  "Whether a command's buffer is killed once the command has finished.

A non-nil value kills the buffer as soon as the command exits, restoring
whatever its window was showing beforehand.  Set this to the symbol
`on-success' to hold on to the buffer of a command that exited non-zero,
so that the error stays readable, or to nil to always keep it.  Commands
whose whole purpose is their output, such as
`system-install-package-info', are never killed.")

(defvar-local system-install--keep-buffer nil
  "Non-nil if this buffer should outlive the command running in it.")

(defun system-install--kill-buffer-p (proc)
  "Return non-nil if the buffer of PROC should be killed now that it exited."
  (and system-install-kill-buffer-when-finished
       (not (buffer-local-value 'system-install--keep-buffer (process-buffer proc)))
       (or (not (eq system-install-kill-buffer-when-finished 'on-success))
           (and (eq (process-status proc) 'exit)
                (eq (process-exit-status proc) 0)))))

(defun system-install--quit-buffer (buf)
  "Kill BUF, quitting any window that displays it."
  (dolist (win (get-buffer-window-list buf nil t))
    ;; The first `quit-restore-window' kills BUF, so re-check each window
    ;; rather than quitting one that has since moved on to another buffer.
    (when (and (window-live-p win) (eq (window-buffer win) buf))
      (quit-restore-window win 'kill)))
  (when (buffer-live-p buf)
    (kill-buffer buf)))

(defun system-install--sentinel (proc event)
  "Report EVENT for PROC and make its buffer dismissable with \\=`q\\='."
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq mode-line-process nil)
        (save-excursion
          (goto-char (point-max))
          (let ((inhibit-read-only t))
            (insert (format "\n%s: %s\n" (process-name proc) (string-trim event)))))
        (system-install--run-minor-mode 1))
      (when (and (memq (process-status proc) '(exit signal))
                 (system-install--kill-buffer-p proc))
        (system-install--quit-buffer buf)))))

(defun system-install--run-buffer (name)
  "Return an empty buffer in which to run NAME.

Reuses the conventional buffer name unless a command is still running
there, in which case a fresh buffer is used instead."
  (let* ((bufname (format "*system install %s*" name))
         (buf (get-buffer bufname)))
    (if (and buf (process-live-p (get-buffer-process buf)))
        (generate-new-buffer bufname)
      (with-current-buffer (get-buffer-create bufname)
        (let ((inhibit-read-only t))
          (erase-buffer))
        (current-buffer)))))

(cl-defun system-install--run (subcmd &key args noroot keep)
  "Execute a system installation subcommand asynchronously.

Execute the given SUBCMD with optional ARGS.

If NOROOT is non-nil, do not use `system-install-sudo-command'.

If KEEP is non-nil, the output buffer survives the command finishing,
regardless of `system-install-kill-buffer-when-finished'."
  (let* ((command
          (string-join
           (flatten-list
            (list (unless noroot system-install-sudo-command)
                  (system-install--get-package-cmd)
                  ;; SUBCMD is a literal flag string, ARGS are package names
                  ;; that came from the outside and need quoting.
                  (split-string subcmd)
                  (cond ((null args) nil)
                        ((listp args) (mapcar #'shell-quote-argument args))
                        (t (shell-quote-argument args)))))
           " "))
         (buf (system-install--run-buffer (car (split-string subcmd)))))

    (with-current-buffer buf
      ;; `shell-command-mode' derives from `comint-mode', which gives us ANSI
      ;; colours, carriage-motion handling and interactive input.
      (shell-command-mode)
      (setq default-directory (expand-file-name "~/"))
      (setq system-install--output-tail "")
      (setq system-install--password-pending nil)
      (setq system-install--keep-buffer keep)
      ;; Detect password prompts ourselves; the stock watcher would otherwise
      ;; double-prompt on the chunks it does manage to match.  Built from the
      ;; global value rather than the current one: this variable is
      ;; `permanent-local', so it survives both `erase-buffer' and the
      ;; major mode call above when this buffer gets reused for a later
      ;; command, and consing onto it would install a second copy of the
      ;; watcher on every run.
      (setq-local comint-output-filter-functions
                  (cons #'system-install--watch-for-password
                        (remq #'comint-watch-for-password-prompt
                              (default-value 'comint-output-filter-functions))))
      (setq mode-line-process '(":%s"))
      (let* ((process-environment
              (append (list (concat "SUDO_PROMPT=" system-install-sudo-prompt))
                      (comint-term-environment)
                      process-environment))
             (proc (start-process-shell-command "system-install" buf command)))
        (set-process-filter proc #'comint-output-filter)
        (set-process-sentinel proc #'system-install--sentinel)))

    (display-buffer buf)
    buf))

;;;###autoload
(defun system-install-clean-cache ()
  "Clean system package cache."
  (interactive)
  (system-install--run (system-install--get-clean-cache-flag)))

;;;###autoload
(defun system-install (package)
  "Install PACKAGE via system installer."
  (interactive
   (list (completing-read "Formula: " (system-install--get-package-list) nil t)))
  (system-install--run (system-install--get-package-install-flag) :args package))

;;;###autoload
(defun system-install-upgrade-package (package)
  "Upgrade PACKAGE to the latest version."
  (interactive
   (list (completing-read "Formula: " (system-install--get-installed-package-list) nil t)))
  (system-install--run (system-install--get-package-update-flag) :args package))

;;;###autoload
(defun system-install-remove-package (package)
  "Remove PACKAGE using system package manager."
  (interactive
   (list (completing-read "Formula: " (system-install--get-installed-package-list) nil t)))
  (system-install--run (system-install--get-package-remove-flag) :args package))

;;;###autoload
(defun system-install-upgrade ()
  "Upgrade all system packages."
  (interactive)
  (system-install--run (system-install--get-system-upgrade-flag)))

;;;###autoload
(defun system-install-update ()
  "Update the package database."
  (interactive)
  (system-install--run (system-install--get-package-update-flag)))

;;;###autoload
(defun system-install-package-info (package)
  "Display \\='info\\=' output for PACKAGE."
  (interactive
   (list (completing-read "Formula: " (system-install--get-package-list) nil t)))
  (system-install--run (system-install--get-package-info-flag)
                       :args package :noroot t :keep t))

(defun system-install--annotator-function (cand)
  "Marginalia annotator CAND."
  (marginalia--fields
   ((system-install--get-package-description cand))))

(defvar system-install-auto-refresh-interval (* 60 60 24)
  "Period at which to auto-refresh the package database.")

(defun system-install-refresh-cache-async ()
  "Asynchronously refresh the package cache."
  (async-start `(lambda ()
                  (setq start-time (current-time))
                  (load ,(locate-library "marginalia"))
                  (load ,(locate-library "s"))
                  (load ,(locate-library "system-install"))

                  (setq system-install--package-cache-file ,system-install--package-cache-file)
                  (setq system-install--package-description-cache-file ,system-install--package-description-cache-file)

                  (require 'system-install)
                  (system-install-refresh-cache)
                  start-time)
               (lambda (start-time) (message (format  "Package refresh finished in %d seconds."
                                                      (float-time (time-subtract (current-time) start-time)))))))

;;;###autoload
(defun system-install-auto-refresh ()
  "Setup an auto refresh timer.

Defaults to once per day but the timer can be modified by modifying
`system-install-auto-refresh-interval'."
  (interactive)
  (run-with-timer 0 3600 'system-install-refresh-cache-async))

(add-to-list 'marginalia-annotators '(system-install-category system-install--annotator-function none))
(add-to-list 'marginalia-command-categories '(system-install . system-install-category))
(add-to-list 'marginalia-command-categories '(system-install-remove-package . system-install-category))
(add-to-list 'marginalia-command-categories '(system-install-upgrade-package . system-install-category))

(provide 'system-install)
;;; system-install.el ends here
