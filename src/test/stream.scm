
(import (aux unittest) (aux base) (aux stream))

(define-suite stream-suite

  ((test/stream/nats _)
   (let1 (nats (take§ 20 (nats§ 0)))
         (⊦= '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19) (§->list nats))))

  ((test/stream/fibs _)
   (let1 (fibs (take§ 20 (fibs§ 0 1)))
         (⊦= '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181) (§->list fibs))))

  ((test/stream/ones _)
   (let1 (ones (take§ 20 (const§ 1)))
         (⊦= '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) (§->list ones))))

  ((test/stream/primes _)
   (let1 (primes (take§ 20 primes§))
         (⊦= '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71) (§->list primes))))

  ((test/stream/long-prefixes _) ; nats§ is linear, so long prefixes are cheap
   (⊦= 9999 (list-ref (§->list (take§ 10000 (nats§ 0))) 9999))
   (⊦= 541 (list-ref (§->list (take§ 100 primes§)) 99)))

  ((test/stream/ones+τ _)
   (let1 (ones (take§ 20 (thunk§ 1)))
         (⊦= '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) (§->list ones))))


  )

(unittest/✓ stream-suite)