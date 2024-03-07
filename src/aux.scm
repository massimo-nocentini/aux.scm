
(module aux *

  (import scheme (chicken base) (chicken continuation) (chicken condition))

  (define-syntax let1 
    (syntax-rules ()
      ((let1 (var val) body ...) (let ((var val)) body ...))))

  (define-syntax letcc 
    (syntax-rules ()
      ((letcc hop body ...) 
       (continuation-capture (lambda (cont)
                              (let ((hop (lambda args (apply continuation-return cont args))))
                                body ...))))))

  (define-syntax trycc
    (syntax-rules (else)
      ((trycc (next exp ...) ... (else e ...))
        (letcc success (letcc next (success (begin exp ...))) ... (begin e ...)))))

  (define-syntax letcar&cdr
    (syntax-rules ()
      ((letcar&cdr (((a d) expr) ...) body ...)
        (let* ((x expr) ...
               (a (car x)) ...
               (d (cdr x)) ...)
            body ...))))
)