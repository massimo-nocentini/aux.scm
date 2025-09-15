
(module (aux fds queue) *

  (import scheme (chicken base) (aux base))

  (define fds-queue-empty (cons 0 (cons '() '())))

  (define (fds-queue-size q) (car q))

  (define (fds-queue-empty? q) (null? (cadr q)))

  (define (fds-queue-car q) (caadr q))

  (define (fds-queue-check size front rear)
    (cons size (cond
                 ((null? front) (cons (reverse rear) '()))
                 (else (cons front rear)))))

  (define-syntax letqueuefront&rear
    (syntax-rules ()
      ((_ () body ...) (begin body ...))
      ((_ (((size front rear) expr) ((ssize ffront rrear) eexpr) ...) body ...)
       (letcar&cdr (((size repr) expr))
                   (letcar&cdr (((front rear) repr))
                               (letqueuefront&rear (((ssize ffront rrear) eexpr) ...) body ...))))))

  (define (fds-queue-cdr q)
    (letqueuefront&rear (((size front rear) q))
                        (fds-queue-check (sub1 size) (cdr front) rear)))

  (define (fds-queue-cons x q)
    (letqueuefront&rear (((size front rear) q))
                        (fds-queue-check (add1 size) front (cons x rear))))

  (define (fds-queue->list q)
    (letqueuefront&rear (((size front rear) q))
                        (append front (reverse rear))))

  (define (list->fds-queue lst)
    (let loop ((lst lst) (q fds-queue-empty))
      (cond
        ((null? lst) q)
        (else (loop (cdr lst) (fds-queue-cons (car lst) q))))))

  (define-syntax letqueue
    (syntax-rules ()
      ((_ ((q expr) ...) body ...) (let ((q (list->fds-queue expr)) ...) body ...))))

  )