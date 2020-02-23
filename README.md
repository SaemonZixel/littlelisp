# Little Lisp

A mini Lisp interpreter in JavaScript. Supports lists (obvs), function invocation, lambdas, lets, if statements, numbers, strings. Use window as default scope. Can call a JS functions and read object fields, call object methods.

* Original author: Mary Rose Cook <mary@maryrosecook.com>

## Examples

```lisp
1
```

```lisp
(
	(let x "ok!") 
	(window.alert x)
)
```

```lisp
(
	(let x "ok!") 
	(window.alert (x.substring 0 2))
)
```

```lisp
((let x 1) (if (x) (alert "x == 1") (alert "x != 1")))
```

```lisp
((lambda (x) (window.alert x)) ("a" "b" "c"))
```

```lisp
(
	(let f1 (lambda (x y) (alert x)))
	(f1 "f1 - ok!")
)
```

