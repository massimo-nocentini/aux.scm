; Serves the Dice of Doom of "Land of Lisp" (Conrad Barski, chapters 19 and 20), as made deployable by
; (aux games dice-of-doom app), on spiffy, for ever:
;
;   csi -s dice-of-doom-server.scm [PORT]
;
; then open http://localhost:PORT/game.html. The port is the first argument, else the environment
; variable PORT, else 8080; the server listens on every interface, or on the address in the
; environment variable HOST (HOST=127.0.0.1 for this machine alone). spiffy writes the errors of
; requests to the standard error.
;
; CHICKEN seeds `pseudo-random-integer`, which deals the boards and rolls the dice, from the clock
; in whole seconds: two servers started in the same second would roll the same dice, and the start
; time would tell the rolls. So the generator is seeded from the system's entropy first; the game
; ids come from `random-bytes` anyway.

(import scheme
        (chicken base)
        (chicken random)
        (chicken bytevector)
        (chicken process-context)
        (aux games dice-of-doom app))

(define (port-number s)
  (let ((n (and s (string->number s))))
    (and n (exact? n) (integer? n) (< 0 n 65536) n)))

(define port
  (let ((given (if (pair? (command-line-arguments)) (car (command-line-arguments)) (get-environment-variable "PORT"))))
    (cond
      ((not given) 8080)
      ((port-number given))
      (else (display (string-append "dice-of-doom-server: not a port: " given "\n") (current-error-port))
            (exit 1)))))

(define host (get-environment-variable "HOST"))

(set-pseudo-random-seed! (random-bytes (make-bytevector 8)))     ; 8 bytes, all a seed is read for

(print "Dice of Doom on http://" (or host "localhost") ":" port "/game.html")
(flush-output)
(dod-serve port: port bind-address: host)
