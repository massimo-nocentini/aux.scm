
(import (aux unittest) (aux base) (aux commons) (aux machine env) (aux machine sc)
        (chicken port) (chicken format) (chicken irregex) (chicken condition))

; The traces print procedures and a gensym'd `apply` marker, whose printed names depend on the
; CHICKEN build (`#<procedure (f a216)>` and `apply0` on CHICKEN 5). Both the original expected
; strings, kept verbatim, and the actual ones are normalised by `normalise` before comparison.
(define normalise
  (λ (str)
    (irregex-replace/all "apply[0-9]+"
                         (irregex-replace/all "#<procedure[^>]*>" str "#<procedure>")
                         "apply")))

(define ⊦=/normalised
  (λ (expected actual)
    (⊦= (normalise expected) (normalise actual))))

(define last* (λ (l) (car (reverse l))))
(define →/interpreted-step →/interpreted) ; the test below shadows `→/interpreted`

(define E₁ ((extend E₀) '(a . 3)))
(define E₂ ((extend E₁) '(b . 4)))

; from on-scheme SC-machine-test; its environment checks are in test/machine.env.scm.
(define-suite sc-machine-suite

  ((doc r) `((structure/section "The SC machine")
             (p "Ported from on-scheme's " (code/inline "SC-machine.scm") ".")
             (structure/section "Implementation")
             (code/scheme/file "../aux.machine.sc.scm")))

  ((test/value _)
   (⊦= 3 ((value E₂) (Id 'a)))
   (⊦= 4 ((value ((extend E₂) `(add1 . ,add1)))
          (Comb (Id 'add1) (Id 'a)))))

  ((test/curryfy _)
   (⊦=
     (Comb
       (Comb
         (Id 'p)
         (Comb
           (Comb
             (Id 'm)
             (Comb
               (Comb (Id 'p) (Id 'a))
               (Id 'b)))
           (Id 'c)))
       (Comb
         (Comb (Id 'f) (Id 'a))
         (Id 'c)))
     (curryfy '(p (m (p a b) c) (f a c))))

   (⊦=
     (Comb
       (Comb
         (Id 'p)
         (Comb
           (Comb (Id 'f) (Id 'a))
           (Id 'c)))
       (Comb
         (Comb (Id 'm) (Id 'c))
         (Comb
           (Comb (Id 'p) (Id 'b))
           (Id 'a))))
     (curryfy '(p (f a c) (m c (p b a))))))

  ((test/printer _)
   (⊦= "((p a) b)"
       (with-output-to-string (τ (display (curryfy '(p a b))))))
   (⊦= "((p ((f a) c)) ((m c) ((p b) a)))"
       (with-output-to-string (τ (display (curryfy '(p (f a c) (m c (p b a)))))))))

  ((test/traces _)
   (let* ((p (λ (a) (λ (b) (+ a b))))
          (m (λ (a) (λ (b) (- a b))))
          (f (λ (a) (λ (b) (+ (² a) (² b)))))
          (control (curryfy '(p (m (p a b) c) (f a c))))
          (control₁ (curryfy '(p (f a c) (m c (p b a)))))
          (E ((extend E₀) `(a . 1) `(b . 2) `(c . 3) `(p . ,p) `(m . ,m) `(f . ,f)))
          (→/interpreted (rtc (→/interpreted E)))
          (→/compiled (rtc (→/compiled E)))
          (F (λ (s) (format #t "~a\n" s))))

     (⊦=/normalised
       "(() (((p ((m ((p a) b)) c)) ((f a) c))))\n(() (((f a) c) (p ((m ((p a) b)) c)) apply0))\n(() (c (f a) apply0 (p ((m ((p a) b)) c)) apply0))\n((3) ((f a) apply0 (p ((m ((p a) b)) c)) apply0))\n((3) (a f apply0 apply0 (p ((m ((p a) b)) c)) apply0))\n((1 3) (f apply0 apply0 (p ((m ((p a) b)) c)) apply0))\n((#<procedure (f a216)> 1 3) (apply0 apply0 (p ((m ((p a) b)) c)) apply0))\n((#<procedure (f_671 b217)> 3) (apply0 (p ((m ((p a) b)) c)) apply0))\n((10) ((p ((m ((p a) b)) c)) apply0))\n((10) (((m ((p a) b)) c) p apply0 apply0))\n((10) (c (m ((p a) b)) apply0 p apply0 apply0))\n((3 10) ((m ((p a) b)) apply0 p apply0 apply0))\n((3 10) (((p a) b) m apply0 apply0 p apply0 apply0))\n((3 10) (b (p a) apply0 m apply0 apply0 p apply0 apply0))\n((2 3 10) ((p a) apply0 m apply0 apply0 p apply0 apply0))\n((2 3 10) (a p apply0 apply0 m apply0 apply0 p apply0 apply0))\n((1 2 3 10) (p apply0 apply0 m apply0 apply0 p apply0 apply0))\n((#<procedure (p a210)> 1 2 3 10) (apply0 apply0 m apply0 apply0 p apply0 apply0))\n((#<procedure (f_657 b211)> 2 3 10) (apply0 m apply0 apply0 p apply0 apply0))\n((3 3 10) (m apply0 apply0 p apply0 apply0))\n((#<procedure (m a213)> 3 3 10) (apply0 apply0 p apply0 apply0))\n((#<procedure (f_664 b214)> 3 10) (apply0 p apply0 apply0))\n((0 10) (p apply0 apply0))\n((#<procedure (p a210)> 0 10) (apply0 apply0))\n((#<procedure (f_657 b211)> 10) (apply0))\n((10) ())\n"
       (with-output-to-string (τ ((fmap F)
                                  (→/interpreted
                                    (make-status '() (list control)))))))

     (⊦=/normalised
       "(() ((Load 3) (Load 1) (Load #<procedure (f a216)>) Apply Apply (Load 3) (Load 2) (Load 1) (Load #<procedure (p a210)>) Apply Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((3) ((Load 1) (Load #<procedure (f a216)>) Apply Apply (Load 3) (Load 2) (Load 1) (Load #<procedure (p a210)>) Apply Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((1 3) ((Load #<procedure (f a216)>) Apply Apply (Load 3) (Load 2) (Load 1) (Load #<procedure (p a210)>) Apply Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((#<procedure (f a216)> 1 3) (Apply Apply (Load 3) (Load 2) (Load 1) (Load #<procedure (p a210)>) Apply Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((#<procedure (f_671 b217)> 3) (Apply (Load 3) (Load 2) (Load 1) (Load #<procedure (p a210)>) Apply Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((10) ((Load 3) (Load 2) (Load 1) (Load #<procedure (p a210)>) Apply Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((3 10) ((Load 2) (Load 1) (Load #<procedure (p a210)>) Apply Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((2 3 10) ((Load 1) (Load #<procedure (p a210)>) Apply Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((1 2 3 10) ((Load #<procedure (p a210)>) Apply Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((#<procedure (p a210)> 1 2 3 10) (Apply Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((#<procedure (f_657 b211)> 2 3 10) (Apply (Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((3 3 10) ((Load #<procedure (m a213)>) Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((#<procedure (m a213)> 3 3 10) (Apply Apply (Load #<procedure (p a210)>) Apply Apply))\n((#<procedure (f_664 b214)> 3 10) (Apply (Load #<procedure (p a210)>) Apply Apply))\n((0 10) ((Load #<procedure (p a210)>) Apply Apply))\n((#<procedure (p a210)> 0 10) (Apply Apply))\n((#<procedure (f_657 b211)> 10) (Apply))\n((10) ())\n"
       (with-output-to-string (τ ((fmap F)
                                  (→/compiled
                                    (make-status '() ((compile E) control)))))))

     (⊦=/normalised
       "(() (((p ((f a) c)) ((m c) ((p b) a)))))\n(() (((m c) ((p b) a)) (p ((f a) c)) apply0))\n(() (((p b) a) (m c) apply0 (p ((f a) c)) apply0))\n(() (a (p b) apply0 (m c) apply0 (p ((f a) c)) apply0))\n((1) ((p b) apply0 (m c) apply0 (p ((f a) c)) apply0))\n((1) (b p apply0 apply0 (m c) apply0 (p ((f a) c)) apply0))\n((2 1) (p apply0 apply0 (m c) apply0 (p ((f a) c)) apply0))\n((#<procedure (p a210)> 2 1) (apply0 apply0 (m c) apply0 (p ((f a) c)) apply0))\n((#<procedure (f_657 b211)> 1) (apply0 (m c) apply0 (p ((f a) c)) apply0))\n((3) ((m c) apply0 (p ((f a) c)) apply0))\n((3) (c m apply0 apply0 (p ((f a) c)) apply0))\n((3 3) (m apply0 apply0 (p ((f a) c)) apply0))\n((#<procedure (m a213)> 3 3) (apply0 apply0 (p ((f a) c)) apply0))\n((#<procedure (f_664 b214)> 3) (apply0 (p ((f a) c)) apply0))\n((0) ((p ((f a) c)) apply0))\n((0) (((f a) c) p apply0 apply0))\n((0) (c (f a) apply0 p apply0 apply0))\n((3 0) ((f a) apply0 p apply0 apply0))\n((3 0) (a f apply0 apply0 p apply0 apply0))\n((1 3 0) (f apply0 apply0 p apply0 apply0))\n((#<procedure (f a216)> 1 3 0) (apply0 apply0 p apply0 apply0))\n((#<procedure (f_671 b217)> 3 0) (apply0 p apply0 apply0))\n((10 0) (p apply0 apply0))\n((#<procedure (p a210)> 10 0) (apply0 apply0))\n((#<procedure (f_657 b211)> 0) (apply0))\n((10) ())\n"
       (with-output-to-string (τ ((fmap F)
                                  (→/interpreted
                                    (make-status '() (list control₁)))))))

     ; new, structural: the normalisation above hides which procedure sits on the stack, so the
     ; trace lengths and the final statuses are checked too.
     (let ((t (→/interpreted (make-status '() (list control))))
           (t/compiled (→/compiled (make-status '() ((compile E) control))))
           (t₁ (→/interpreted (make-status '() (list control₁)))))
       (⊦= 26 (length t))
       (⊦= 18 (length t/compiled))
       (⊦= 26 (length t₁))
       (⊦= '((10) ()) (let1 (s (last* t)) (list (status-S s) (status-C s))))
       (⊦= '((10) ()) (let1 (s (last* t/compiled)) (list (status-S s) (status-C s))))
       (⊦= '((10) ()) (let1 (s (last* t₁)) (list (status-S s) (status-C s))))
       ; the fixed point is the very same status object
       (let1 (s (last* t))
         (⊨ (eq? s ((→/interpreted-step E) s)))))))

  ; new: wrong variants are rejected, as the old `datatype` did.
  ((test/strict-constructors _)
   (⊨ (condition-case (begin (Id "a") #f) ((exn) #t)))
   (⊨ (condition-case (begin (Comb 'a (Id 'b)) #f) ((exn) #t)))
   (⊨ (condition-case (begin (Load 3) #f) ((exn) #t)))
   (⊨ (condition-case (begin ((value E₀) 'a) #f) ((exn) #t))))

  )

(unittest/✓ sc-machine-suite)
