# CL-ENVY

A Collection of generic and useful Common Lisp conveniences. If you
don't like them, don't use them.

## Usage

### /. - A Shen-inspired Terse Lambda syntax

This macro expands to normal lambdas. The arguments work just like a
normal lambda or function arguments, but they are not surrounded by an
extra set of parenthesis, instead the last element is considered to be
the body of the function, unless there is a "->" symbol in the list,
then all the succeeding elements are treated like an implicit PROGN.

Additionally, any or even multiple arguments can be explicitely ignored
by making them underscores.

Example of basic usage:

```common-lisp
(/.)
;; expands to
(LAMBDA ())

(/. x)
;; expands to
(LAMBDA () X)

(/. x x)
;; expands to
(LAMBDA (X) X)

(/. x y &rest rest (apply #'* x y rest))
;; expands to
(LAMBDA (X Y &REST REST)
  (APPLY #'* X Y REST)) 
```

Here are some examples with the ignored parameters:

```common-lisp
(/. x _ (+ x 10))
;; expands to
(LAMBDA (X #:UNUSED781)
  (DECLARE (IGNORE #:UNUSED781))
  (+ X 10))

(/. _ x &rest _ (+ x 10))
;; expands to
(LAMBDA (#:UNUSED782 X &REST #:UNUSED783)
    (DECLARE (IGNORE #:UNUSED782 #:UNUSED783))
    (+ X 10))
```

Here are some examples with the implicit PROGN using the "->".

```common-lisp
(/. x y z ->
  (format t "debug~%")
  (* x y z))
;; expands to
(LAMBDA (X Y Z)
  (FORMAT T "debug~%")
  (* X Y Z))

(/. -> x y z)
;; expands to
(LAMBDA () X Y Z)
```

The purpose of the terse lambda syntax is mostly to have a way to define
short anonymous functions a little shorter. There is no limitation on
using normal argument lists other than not being able to use the special
"_" and "->" symbols. The macro does nothing to the body of the function
other than prepend any DECLARE IGNORE.

### bnd - A terse replacement for LET\* and MULTIPLE-VALUE-BIND

First, think of LET* with none of the grouping parens, instead, the
first elem gets bound to the second and so on, and the last element is
the "body", and just like /. above, an implicit PROGN can be signalled
with the "->" symbol.

The second trick is that if the binding name is not a symbol, but a
list, it gets turned into a MULTIPLE-VALUE-BIND automatically.

The last trick is that "_" parameters are explicitely ignored.

Here is an extremely simple usage:

```common-lisp
(bnd x 10 y 20 (+ x y))
;; expands to:
(LET ((X 10))
  (LET ((Y 20))
    (+ X Y)))
```

Now, with "->" for implicit PROGN:

```common-lisp
(bnd x 10 y 20 -> (format t "hello") (+ x y))
;; expands to:
(LET ((X 10))
  (LET ((Y 20))
    (FORMAT T "HELLO")
    (+ X Y)))
```

With implicit MULTIPLE-VALUE-BIND:

```common-lisp
(bnd x 10
     (y z) (floor 5 x)
  (+ x z))
;; expands to:
(LET ((X 10))
  (MULTIPLE-VALUE-BIND (Y Z) (FLOOR 5 X)
    (+ X Z)))
```

With ignored "_" parameters.

```common-lisp
(bnd x 10
     (_ z) (floor 5 x)
  (+ x z))
;; expands to:
(LET ((X 10))
  (MULTIPLE-VALUE-BIND (#:UNUSED767 Z)
      (FLOOR 5 X)
    (DECLARE (IGNORE #:UNUSED767))
    (+ X Z)))
```

## Installation

## Author

* Stephen A. Goss (steveth45@gmail.com)

## Copyright

Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)

# License

Licensed under the Modified BSD License.

