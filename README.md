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

    :argdo %s/vim/emacs/g

Revert changes made from a previous `:argdo`

    :argdo edit!

# Substitue `##` with the arglist in Ex

Support is also added for the special file name `##` which will be replaced
with a concatentation of all the files in the argument list, separated by
spaces, when it appears in an Ex command.
