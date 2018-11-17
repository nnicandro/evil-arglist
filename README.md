Implements most aspects of the argument list feature from Vim in Evil. An
argument list is a set of file paths on which you can perform operations.

# Installation

```elisp
(add-to-list 'load-path "~/.emacs.d/path/to/evil-arglist")
(require 'evil-arglist)
```

# Examples

Set the argument list to all .el files in the current directory

    :args *.el

Show all files in the argument list

    :args

Edit the next file on the argument list

    :n

Edit the previous file on the argument list

    :p

Delete all files from the argument list

    :argd *

Run checkdoc on every file in the argument list

    :argdo checkdoc

Search and replace

    :argdo s/vim/emacs/g

Revert changes made from a previous `:argdo`

    :argdo edit!

# `:argdo` and Ex ranges

When using `:argdo`, `evil-ex-range` is set to encompase the whole buffer. This
allows commands like `:argdo s/vim/emacs/g` to operate on the whole buffer, but
makes commands like `:argdo w` work differently than expected since those
commands have different behavior depending on if an Ex range was specified or
not. This is only an issue if you supply an Ex command to `:argdo`, there is no
such problem if a non-Ex command is provided like `checkdoc`.
