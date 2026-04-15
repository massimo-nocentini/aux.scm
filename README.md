# aux.scm

In order to install the `aux` modules just type in terminal:

```bash
make install -sudo
```

provided that a Chicken scheme is installed on your box.

## Included modules

The repository contains a collection of small CHICKEN Scheme modules, including
functional data structures, stream utilities, nondeterministic programming
helpers, and foreign-function wrappers.

### `(aux fds sbral)`

`(aux fds sbral)` implements a skew binary random-access list: a persistent
sequence that keeps constant-time access to the front and logarithmic indexed
lookup and update.

Exported entry points include:

- `empty/sbral`
- `cons/sbral`, `car/sbral`, `cdr/sbral`
- `sbral-ref`, `update/sbral`
- `length/sbral`
- `list->sbral`, `sbral->list`
- `map/sbral`, `filter/sbral`, `exists?/sbral`

Example:

```scheme
(import (aux fds sbral))

(define values (list->sbral '(a b c d)))

(sbral-ref values 2)
;; => c

(sbral->list (update/sbral 1 'B values))
;; => (a B c d)

(sbral->list (cons/sbral 'z values))
;; => (z a b c d)
```