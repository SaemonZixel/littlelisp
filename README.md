# Little Lisp

A mini Lisp interpreter in JavaScript.  Supports lists (obvs), function invocation, lambdas, lets, if statements, numbers, strings and the library functions `first`, `rest` and `print`.

* Original author: Mary Rose Cook <mary@maryrosecook.com>
* Thanks to Martin Tornwall for the implementations of let and if.

## Examples

```lisp
1
```

```lisp
(first (1 2 3))
```

```lisp
((lambda (x) (rest x)) ("a" "b" "c"))
```

```lisp
((let x "ok!") (print x))
```

```lisp
((let x 1) (if (x) (print "x == 1") (print "x != 1")))
```
