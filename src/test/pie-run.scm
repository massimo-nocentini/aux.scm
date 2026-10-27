;;; pie-run.scm, the command-line runner of (aux pie), the counterpart of `racket FILE.pie`. Run it as
;;; `csi -s pie-run.scm FILE.pie`. It prints what Racket Pie prints and exits 0, or 1 after an error. Part
;;; of the port of Pie (https://github.com/the-little-typer/pie), distributed under the GNU AGPL-3.0.

(import (aux pie) (only (chicken process-context) command-line-arguments))

(pie-main (command-line-arguments))
