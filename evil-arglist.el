;;; evil-arglist.el --- Add argument lists to Evil -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 17 Nov 2018
;; Version: 0.0.1
;; Keywords: emulations
;; Package-Requires: ((emacs "24.3") (evil "1.2.13"))
;; URL: https://github.com/dzop/evil-arglist

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Implements most aspects of the argument list feature from Vim in Evil. An
;; argument list is a set of file paths on which you can perform operations.
;;
;; Examples:
;;
;; Set the argument list to all .el files in the current directory
;;
;;     :args *.el
;;
;; Show all files in the argument list
;;
;;     :args
;;
;; Edit the next file on the argument list
;;
;;     :n
;;
;; Edit the previous file on the argument list
;;
;;     :p
;;
;; Delete all files from the argument list
;;
;;     :argd *
;;
;; Run checkdoc on every file in the argument list
;;
;;     :argdo checkdoc
;;
;; Search and replace
;;
;;     :argdo s/vim/emacs/g
;;
;; Revert changes made from a previous :argdo
;;
;;     :argdo edit!

;;; Code:

(require 'evil)

(defgroup evil-arglist nil
  "Argument lists in Evil."
  :group 'evil)

(defvar evil-arglist nil
  "The global argument list.
An argument list is a cons cell, (IDX . ARGS), where IDX is the
current index of the file being edited in the list of file
ARGS.")

(add-to-list 'window-persistent-parameters (cons 'evil-arglist t))

(defun evil-arglist-construct-initial ()
  "Add the dynamically bound variable ARGI to the `evil-arglist'."
  ;; Don't consider the file processed
  (prog1 nil
    (when argi
      (let ((files (cdr (evil-arglist-get)))
            (file (expand-file-name argi)))
        (evil-arglist-set
         (if files (nconc files (list file))
           (list file)))))))

(defun evil-arglist-rewind-safe ()
  "Call `evil-arglist-rewind' if files exist on the arglist."
  (unless (zerop (evil-arglist-length))
    (evil-arglist-rewind)))

;; When `command-line-functions' is called, argi can only ever be parsed as a
;; file name.
(add-hook 'command-line-functions 'evil-arglist-construct-initial)
(add-hook 'emacs-startup-hook 'evil-arglist-rewind-safe)

;;; Manipulating the argument list

(defun evil-arglist-get ()
  "Return the current argument list.
If the `selected-window' has a non-nil evil-arglist window
parameter return it, otherwise return the value of the variable
`evil-arglist'."
  (or (window-parameter nil 'evil-arglist) evil-arglist
      (setq evil-arglist (cons -1 nil))))

(defun evil-arglist-length ()
  "Return the number of files in the argument list."
  (length (cdr (evil-arglist-get))))

(defun evil-arglist-nth (n &optional relative)
  "Return the file name of the Nth file on the argument list.
If RELATIVE is non-nil, return the file name of the Nth file
relative to the current argument.

Return nil if N is out of bounds."
  (cl-destructuring-bind (curr-idx . files)
      (evil-arglist-get)
    (let ((idx (+ n (if relative curr-idx 0))))
      (and (>= idx 0) (nth idx files)))))

(defun evil-arglist-set (files &optional file)
  "Set the current argument list to FILES.
If the `selected-window' has a non-nil evil-arglist window
parameter, then the window local argument list is updated and any
windows sharing the argument list. Otherwise the global
`evil-arglist' is updated.

FILE is a member of FILES that will be set as the current
position on the argument list."
  ;; Ensure that any window's sharing the argument list are updated
  (setcdr (evil-arglist-get) files)
  (when file
    (evil-arglist-set-current file)))

(defun evil-arglist-set-current (file)
  "Set the current argument to FILE in the argument list.
Raise an error if FILE does not exist in the argument list."
  (let* ((arglist (evil-arglist-get))
         (files (cdr arglist))
         ;; Argument lists members do not have to be unique, so try to get the
         ;; same file if duplicates exist.
         (head (and file (or (memq file files) (member file files))))
         (idx (and head (- (length files) (length head)))))
    (unless head
      (error "%s not in argument list" file))
    ;; Ensure that any window's sharing the argument list are updated
    (setcar arglist idx)))

(defun evil-arglist-do-edit (count &optional relative)
  "Edit the COUNT file on the argument list.
If RELATIVE is non-nil, edit the COUNT argument relative to the
current one. Update the currently edited argument in the argument
list."
  (when (evil-arglist-get)
    (let* ((arg (evil-arglist-nth count relative)))
      (unless arg
        (user-error
         (if (> count 0) "Cannot go beyond last file"
           "Cannot go before first file")))
      (find-file arg)
      (evil-arglist-set-current arg))))

(defadvice split-window-internal (around evil-arglist activate)
  "Split the window and set the arglist for the new window.
If the `selected-window' has a non-nil evil-arglist window
parameter, propagate it to the new window. The argument list is
thus shared between the two of them. Otherwise the behavior is
identical to `split-window-internal'."
  (let* ((window (ad-get-arg 0))
         (new-window ad-do-it))
    (prog1 new-window
      (set-window-parameter
       new-window 'evil-arglist (window-parameter window 'evil-arglist)))))

;;; Interactive codes

(evil-define-interactive-code "<f+>"
  "Ex repeated file argument."
  :ex-arg file+
  (list (when (evil-ex-p)
          ;; TODO: File names with spaces
          (let ((args (evil-ex-file-arg)))
            (and args (split-string args))))))

(evil-define-interactive-code "<cmd>"
  "Ex command argument."
  (list
   (when (evil-ex-p)
     (evil-ex-parse evil-ex-argument nil 'expression))))

(defun evil-arglist-ex-repeated-file-argument-completion ()
  "Enable completing a list of file names in Ex."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward "[^\\] " nil t)
        (list (+ (point) 2) (point-max) 'read-file-name-internal)
      (list (or (and evil-ex-argument
                     (get-text-property 0 'ex-index evil-ex-argument))
                (point-min))
            (point-max)
            'read-file-name-internal))))

(evil-ex-define-argument-type file+
  "Handle a repeated file argument."
  :completion-at-point evil-arglist-ex-repeated-file-argument-completion)

;;; Commands

(evil-ex-define-cmd "ar[gs]" 'evil-arglist)
(evil-ex-define-cmd "arga[dd]" 'evil-arglist-add)
(evil-ex-define-cmd "argd[elete]" 'evil-arglist-delete)
(evil-ex-define-cmd "argdo" 'evil-arglist-do)
(evil-ex-define-cmd "arge[dit]" 'evil-arglist-edit)
(evil-ex-define-cmd "argg[lobal]" 'evil-arglist-global)
(evil-ex-define-cmd "argl[ocal]" 'evil-arglist-local)
(evil-ex-define-cmd "argu[ment]" 'evil-arglist-argument)

(evil-ex-define-cmd "n[ext]" 'evil-arglist-next)
(evil-ex-define-cmd "N[ext]" 'evil-arglist-previous)
(evil-ex-define-cmd "p[revious]" 'evil-arglist-previous)
(evil-ex-define-cmd "rew[ind]" 'evil-arglist-rewind)
(evil-ex-define-cmd "fir[st]" 'evil-arglist-rewind)
(evil-ex-define-cmd "la[st]" 'evil-arglist-last)
(evil-ex-define-cmd "wn[ext]" 'evil-arglist-write-next)
(evil-ex-define-cmd "wN[ext]" 'evil-arglist-write-previous)
(evil-ex-define-cmd "wp[revious]" 'evil-arglist-write-previous)

(defun evil-arglist-files-from-patterns (patterns)
  "Return a list of all files that match PATTERNS.
Any PATTERNS equal to \"%\" are replaced with the
`current-buffer's file name if one exists, otherwise the pattern
is ignored.

For every other pattern, the wildcards are expanded into lists of
files and those files appended together. If a pattern does not
contain any wildcards, it is added to the resulting list without
modification."
  (apply #'append
         (cl-loop
          ;; From `wildcard-to-regexp'
          with wildcard-re = "[[.*+\\^$?]"
          with buf = (buffer-base-buffer)
          for pat in patterns
          if (and (equal pat "%") (buffer-file-name buf))
          collect (list (buffer-file-name buf))
          else collect
          (if (string-match-p wildcard-re pat)
              (cl-remove-if-not #'file-regular-p
                                (file-expand-wildcards pat 'abs))
            (list pat)))))

(evil-define-command evil-arglist (&optional patterns)
  "Show the current argument list or set it to the files matching PATTERN.
If PATTERN is empty, show the current argument list. Otherwise
set the argument list to the list of files matching PATTERN in
the current directory. PATTERN can be any wildcard pattern
recognized by `file-expand-wildcards'."
  :repeat nil
  (interactive "<a>")
  (if (zerop (length patterns))
      (when (evil-arglist-get)
        (cl-destructuring-bind (_ . args) (evil-arglist-get)
          (let ((curr (evil-arglist-nth 0 'relative)))
            (message
             (mapconcat
              (lambda (arg)
                (let ((file (file-relative-name arg)))
                  (if (eq curr arg) (concat "[" file "]") file)))
              args
              " ")))))
    (let ((files (evil-arglist-files-from-patterns (split-string patterns))))
      (unless files
        (user-error "No matching files"))
      (evil-arglist-set files)
      (evil-arglist-do-edit 0))))

(evil-define-command evil-arglist-local (pattern)
  "Make a local copy of the argument list for the `selected-window'."
  (interactive "<a>")
  (set-window-parameter nil 'evil-arglist (copy-sequence evil-arglist))
  (when pattern
    (evil-arglist pattern)))

(evil-define-command evil-arglist-global (pattern)
  "Force the `selected-window' to use the global argument list."
  (interactive "<a>")
  (set-window-parameter nil 'evil-arglist nil)
  (when pattern
    (evil-arglist pattern)))

(defun evil-arglist-set-cmd-range ()
  "Set `evil-ex-range' for the current argument list command."
  (setq evil-ex-range (evil-ex-full-range))
  (remove-hook 'pre-command-hook 'evil-arglist-set-cmd-range t))

(defun evil-arglist-do-cmd (cmd)
  "Evaluate CMD for the current buffer.
CMD is an `evil-ex-call-command' form as returned by
`evil-ex-parse'."
  ;; Ensure that `evil-ex-call-command' doesn't set the visual region
  (let (evil-ex-range)
    ;; But ensure that if CMD is another Ex command, it operates on the whole
    ;; buffer.
    ;;
    ;; FIXME: This has some issues since commands like :w do something
    ;; differently if `evil-ex-range' is non-nil vs when it is nil.
    (add-hook 'pre-command-hook 'evil-arglist-set-cmd-range nil t)
    (eval cmd)))

(evil-define-command evil-arglist-do (cmd)
  "Go to the start of the argument list, evaluate CMD for each file on the list.
Each file visited and set as the current buffer before CMD is
evaluated."
  (interactive "<cmd>")
  (dotimes (i (evil-arglist-length))
    (evil-arglist-do-edit i)
    (evil-arglist-do-cmd cmd)))

(evil-define-command evil-arglist-add (count names)
  "Add NAMES to the argument list.
Insert NAMES after the COUNT argument on the argument list. If
COUNT is omitted, insert NAMES after the current argument. If
NAMES is omitted, add the `buffer-file-name' of the
`current-buffer'."
  (interactive "<c><f+>")
  (let* ((arglist (evil-arglist-get))
         (idx (car arglist))
         (files (cdr arglist)))
    (setq names (if names (mapcar #'expand-file-name names)
                  (list (buffer-file-name (buffer-base-buffer)))))
    (evil-arglist-set
     (cond
      ((null files) names)
      (t
       (or count (setq count (1+ idx)))
       (append (cl-subseq files 0 count)
               names (cl-subseq files count))))
     (nth idx files))))

(evil-define-command evil-arglist-delete (patterns)
  "Delete any files matching PATTERNS in the argument list.
PATTERNS should be a list of wildcard patterns for file names to
match against.

The special pattern % means to delete the file at the current
position on the argument list."
  (interactive "<a>")
  (unless patterns
    (user-error "Argument required"))
  (let* ((arglist (evil-arglist-get))
         (files (cdr arglist)))
    (setq patterns (split-string patterns))
    (when (member "%" patterns)
      (setq patterns (remove "%" patterns)
            files (delq (nth (car arglist) files) files)))
    (evil-arglist-set
     (if patterns
         (cl-loop
          with gen-regexp = (lambda (p)
                              ;; Remove \\` and \\' at the beginning and end of
                              ;; the regexp to match any part of a filename
                              (substring (wildcard-to-regexp p) 2 -2))
          with exclude = (mapconcat #'identity (mapcar gen-regexp patterns) "\\|")
          for file in files if (not (string-match-p exclude file))
          collect file)
       files))))

(evil-define-command evil-arglist-edit (count names)
  "Add NAMES to the argument list.
COUNT has the same meaning as in `evil-arglist-add'."
  (interactive "<c><f+>")
  (unless names (user-error "Argument required"))
  (evil-arglist-add count names)
  (if count (evil-arglist-do-edit (1- count))
    (evil-arglist-next)))

(evil-define-command evil-arglist-argument (count &optional cmd)
  "Edit the COUNT argument, evaluate CMD."
  :keep-visual nil
  (interactive "<c><cmd>")
  (and count (setq count (1- count)))
  (evil-arglist-do-edit (or count 0) (not count))
  (when cmd
    (evil-arglist-do-cmd cmd)))

(evil-define-command evil-arglist-next (&optional count pattern)
  "Edit the COUNT next file on the argument list after the current one."
  (interactive "<c><a>")
  (if pattern (evil-arglist pattern)
    (evil-arglist-do-edit (or count 1) 'relative)))

(evil-define-command evil-arglist-previous (&optional count)
  "Edit the COUNT previous file on the argument list before the current one."
  (interactive "<c>")
  (evil-arglist-do-edit (- (or count 1)) 'relative))

(evil-define-command evil-arglist-rewind ()
  "Edit the first file on the argument list."
  (interactive)
  (evil-arglist-do-edit 0))

(evil-define-command evil-arglist-last ()
  "Edit the last file on the argument list."
  (interactive)
  (evil-arglist-do-edit (1- (evil-arglist-length))))

(evil-define-command evil-arglist-write-next (file)
  "Save the current FILE, edit the next file on the argument list."
  (interactive "<f>")
  (evil-write nil nil nil file)
  (evil-arglist-next))

(evil-define-command evil-arglist-write-previous (file)
  "Save the current FILE, edit the previous file on the argument list."
  (interactive "<f>")
  (evil-write nil nil nil file)
  (evil-arglist-previous))

(defadvice evil-ex-replace-special-filenames (around evil-arglist activate)
  (let ((files (cdr (evil-arglist-get))))
    (when files
      (ad-set-arg
       0 (replace-regexp-in-string
          "\\(?:[[:blank:]]\\|\\`\\)\\(##\\)\\(?:[[:blank:]]\\|\\'\\)"
          (mapconcat #'shell-quote-argument files " ") (ad-get-arg 0)
          t t 1)))
    ad-do-it))

(provide 'evil-arglist)

;;; evil-arglist.el ends here
