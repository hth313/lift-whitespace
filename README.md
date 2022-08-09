Whitespace checker for Sonatype Lift
====================================

This is an extension to Sonatype Lift which detects mis-use of
whitespace. With that I consider space character(s) at end of
line, missing or multiple newlines at the end of a file.

Tab characters inside the files are also flagged, but they are
allowed for `Makefile` or `*.make` files as a leading tab
character is fundamental to `make`.

![](https://lift.sonatype.com/api/badge/github.com/hth313/lift-whitespace)
