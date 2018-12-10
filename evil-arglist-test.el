;;; evil-arglist-test.el --- Tests for evil-arglist -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 10 Dec 2018
;; Version: 0.0.1
;; URL: https://github.com/dzop/evil-arglist

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
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

;;

;;; Code:

(require 'evil-arglist)
(require 'ert)

(ert-deftest evil-arglist-test-set-get ()
  (ert-info ("Setting/getting arglist")
    (let ((evil-arglist (list 1 2 3)))
      (should (eq evil-arglist (evil-arglist-get)))
      (set-window-parameter nil 'evil-arglist evil-arglist)
      (should (eq (window-parameter nil 'evil-arglist) (evil-arglist-get)))
      (set-window-parameter nil 'evil-arglist nil))
    (let ((evil-arglist nil))
      (evil-arglist-set (list 1 2 3) 2)
      (should (equal evil-arglist '(1 1 2 3)))
      (set-window-parameter nil 'evil-arglist (copy-sequence evil-arglist))
      (evil-arglist-set (list 3 2 1) 1)
      ;; Global arglist should be preserved if window local arglist is present
      (should (equal evil-arglist '(1 1 2 3)))
      (should (equal (window-parameter nil 'evil-arglist) '(2 3 2 1)))
      (set-window-parameter nil 'evil-arglist nil))))

(ert-deftest evil-arglist-test-do-edit ()
  (ert-info ("Setting the currently edited argument")
    (cl-letf (((symbol-function 'find-file) 'ignore))
      (let ((evil-arglist (list 0 2 3 1 5)))
        (evil-arglist-do-edit 1)
        (should (equal (car evil-arglist) 1))
        (evil-arglist-do-edit 2 'relative)
        (should (equal (car evil-arglist) 3))
        (should-error (evil-arglist-do-edit 10))
        (should-error (evil-arglist-do-edit -1))))))

(ert-deftest evil-arglist-test-add-args ()
  (let ((default-directory "")
        (evil-arglist (list 1 "./a" "./b")))
    (evil-arglist-add nil '("c" "d" "e"))
    (should (equal evil-arglist '(1 "./a" "./b" "./c" "./d" "./e")))
    (evil-arglist-add 2 '("f"))
    (should (equal evil-arglist '(1 "./a" "./b" "./f" "./c" "./d" "./e")))
    (evil-arglist-add 0 '("g"))
    (should (equal evil-arglist '(2 "./g" "./a" "./b" "./f" "./c" "./d" "./e")))))

(ert-deftest evil-arglist-test-delete-args ()
  (ert-info ("Deleting from the argument list")
    (let ((evil-arglist (list 0 "foo.c" "bar.d" "baz.el")))
      (evil-arglist-delete "*.c")
      (should (equal evil-arglist '(0 "bar.d" "baz.el")))
      (evil-arglist-set (cdr evil-arglist) "baz.el")
      (evil-arglist-delete "%")
      (should (equal evil-arglist '(1 "bar.d"))))))

(ert-deftest evil-arglist-test-motion ()
  (ert-info ("Moving around on the argument list")
    (cl-letf (((symbol-function 'find-file) 'ignore))
      (let ((evil-arglist (list 1 2 1 3 5)))
        (evil-arglist-rewind)
        (should (equal (car evil-arglist) 0))
        (should-error (evil-arglist-previous))
        (evil-arglist-next)
        (should (equal (car evil-arglist) 1))
        (evil-arglist-last)
        (should (equal (car evil-arglist) 3))
        (should-error (evil-arglist-next))
        (evil-arglist-previous)
        (should (equal (car evil-arglist) 2))
        (evil-arglist-argument 1)
        (equal (car evil-arglist) 0)))))

(ert-deftest evil-arglist-test-special-filename ()
  (ert-info ("Support ## in Ex")
    (let ((evil-arglist (list 0 "a" "b" "c"))
          (alt (and (other-buffer)
                    (buffer-file-name (other-buffer)))))
      (should (equal (evil-ex-replace-special-filenames "aa ## bb") "aa a b c bb"))
      (should (equal (evil-ex-replace-special-filenames "aa ##") "aa a b c"))
      (should (equal (evil-ex-replace-special-filenames "## bb") "a b c bb"))
      (should (equal (evil-ex-replace-special-filenames "# ## #")
                     (concat alt " a b c " alt))))))

(ert-deftest evil-arglist-test-+cmd ()
  (let (cmd)
    (with-temp-buffer
      (insert "1\n2\n3")
      (goto-char (point-min))
      (setq cmd (car (evil-parser "+" '+command evil-arglist-grammar)))
      (eval cmd)
      (should (looking-at "3")))
    (with-temp-buffer
      (insert "1\nfoo\n3\n")
      (goto-char (point-min))
      (setq cmd (car (evil-parser "+/foo" '+command evil-arglist-grammar)))
      (eval cmd)
      (should (looking-at "foo\n")))
    (with-temp-buffer
      (insert "1\nfoo\n3\n")
      (goto-char (point-min))
      ;; Set this so that `evil-ex-substitute' will work
      (let ((evil-ex-current-buffer (current-buffer)))
        (setq cmd (car (evil-parser "+%s/foo/bar" '+command evil-arglist-grammar)))
        (eval cmd))
      (should (looking-at "bar\n")))
    (with-temp-buffer
      (insert "1\n2\n3\n")
      (goto-char (point-min))
      (setq cmd (car (evil-parser "+2" '+command evil-arglist-grammar)))
      (eval cmd)
      (should (looking-at "2\n")))))

;;; evil-arglist-test.el ends here
