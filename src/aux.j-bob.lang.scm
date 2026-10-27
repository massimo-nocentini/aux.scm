;;; (aux j-bob lang) -- the language of J-Bob: total primitives, t/nil truth, and defun/dethm.
;;;
;;; A port of scheme/j-bob-lang.scm of J-Bob, the proof assistant of "The Little Prover" by Daniel P.
;;; Friedman and Carl Eastlund (https://github.com/the-little-prover/j-bob, commit 2a4567b). J-Bob is
;;; distributed under the 2-clause BSD license, and so is this file, which is derived from it:
;;;
;;;   Copyright (c) 2015, Daniel P. Friedman and Carl Eastlund
;;;   All rights reserved.
;;;
;;;   Redistribution and use in source and binary forms, with or without modification, are permitted
;;;   provided that the following conditions are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright notice, this list of conditions
;;;   and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright notice, this list of
;;;   conditions and the following disclaimer in the documentation and/or other materials provided with
;;;   the distribution.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
;;;   IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
;;;   FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
;;;   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
;;;   IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
;;;   OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;; Upstream, j-bob-lang.scm is loaded at the top level and redefines car, cdr, +, < and if there, after
;;; it has kept the host's car, cdr, + and < as s.car, s.cdr, s.+ and s.<. A module cannot redefine what
;;; it imports, so here the host's if, car, cdr, + and < are imported with the prefix scheme: and the
;;; module defines its own. The definitions before the new if (upstream lines 6-18) use the host's if,
;;; which upstream they still see, and so here they say scheme:if. Everything else is upstream's text.
;;;
;;; Importing this module gives J-Bob's language, for writing J-Bob programs and for (aux j-bob), which
;;; is written in it. Import it in place of those five names of scheme, as in
;;;   (import (except scheme if car cdr + <) (aux j-bob lang))
;;; Then if has exactly three arms, its question is false only when it is the symbol nil, and #f and '()
;;; count as true; car and cdr answer '() on an atom, + and < count a non-number as 0, and atom, equal,
;;; natp and < answer the symbols t and nil. defun and dethm both become define, so a theorem is a
;;; procedure that answers t or nil for its arguments.

(module (aux j-bob lang)
  ( ; the host's primitives, kept
    s.car s.cdr s.+ s.<
    ; the total primitives
    num if/nil atom car cdr equal natp + <
    ; the syntax
    if defun dethm
    ; the size of a value, the measure of the book's proofs
    size)

  (import (except scheme if car cdr + <)
          (prefix (only scheme if car cdr + <) scheme:))

  (define s.car scheme:car)
  (define s.cdr scheme:cdr)
  (define s.+ scheme:+)
  (define s.< scheme:<)
  (define (num x) (scheme:if (number? x) x 0))
  (define (if/nil Q A E)
    (scheme:if (equal? Q 'nil) (E) (A)))

  (define (atom x) (scheme:if (pair? x) 'nil 't))
  (define (car x) (scheme:if (pair? x) (s.car x) '()))
  (define (cdr x) (scheme:if (pair? x) (s.cdr x) '()))
  (define (equal x y) (scheme:if (equal? x y) 't 'nil))
  (define (natp x)
    (scheme:if (integer? x) (scheme:if (s.< x 0) 'nil 't) 'nil))
  (define (+ x y) (s.+ (num x) (num y)))
  (define (< x y)
    (scheme:if (s.< (num x) (num y)) 't 'nil))

  (define-syntax if
    (syntax-rules ()
      ((_ Q A E)
       (if/nil Q (lambda () A) (lambda () E)))))

  (define-syntax defun
    (syntax-rules ()
      ((_ name (arg ...) body)
       (define (name arg ...) body))))

  (define-syntax dethm
    (syntax-rules ()
      ((_ name (arg ...) body)
       (define (name arg ...) body))))

  (defun size (x)
    (if (atom x)
      '0
      (+ '1 (+ (size (car x)) (size (cdr x)))))))
