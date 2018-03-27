# Emacs package faceheight

Emacs package to automatically set text size in frames based on pixel
pitch and monitor size.

See `faceheight` customization group for variables to control it.
Adjust `faceheight-start-index` if you find the size it chooses too
large or small.  Then it should give you reasonable faces even on
monitors with very different pixel itches or sizes.

If you find it is inconsistent between monitors, please discuss with
me (e.g. github issues) so we can try to work out a better algorithm.

Have it automatically configure frames with:

``` emacs-lisp
(defun my-setup-new-frame (frame)
  (faceheight-fix-frame nil frame))
(push 'my-setup-new-frame after-make-frame-functions)
```

Commands to bind are `faceheight-fix-frame` and/or
`faceheight-fix-all`, plus probably `faceheight-inc-size`.

It meets my needs but may not fit yours - suggestions, issues and
commentary welcome.  If you think it's worth sending to Melpa, let me
know.
