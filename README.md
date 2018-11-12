# Emacs package faceheight

Emacs package to automatically set text size in frames based on pixel
pitch and monitor size to provide appropriate text size.

See `faceheight` customization group for variables to control it.  At
core, it finds an index along a face height array.  Adjust
`faceheight-start-index` if you find the size it chooses too large or
small.  Then it should give you reasonable faces even on monitors with
very different pixel itches or sizes.

If you get inconsistent/undesirable results between monitors, please
discuss with me (e.g. github issues) so we can try to work out a
better algorithm.

Have it automatically configure frames with:

Commands to bind are `faceheight-fix-frame` and/or
`faceheight-fix-all`, plus probably `faceheight-inc-size`.

It meets my needs but may not fit yours - suggestions, issues and
commentary welcome.  I doubt it's coherent and clean enough to live in
Melpa as it currently is.
