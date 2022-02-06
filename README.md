# simple-regexp

This implements a simple NFA-based regular expression matcher, without backtracking. For more information, see the comments in `simple-regexp.lisp`.

## Example Usage

```
SIMPLE-REGEXP> (match #\c "d")
NIL
SIMPLE-REGEXP> (match (alt "dog" "cat" "bird") "bird")
T
SIMPLE-REGEXP> (match (seq "__" (star #'alphanumericp) "__") "__foobar__")
T
```
