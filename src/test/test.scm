
(import (aux unittest) (aux base) (chicken sort) (aux simdjson))

(define-suite auxtest

  ((test/let1 _)
   (let1 (a 1) (⊦= a 1)))

  ((test/letcar&cdr _)
   (letcar&cdr (((a d) (cons 1 '()))
                ((aa dd) (cons 2 3)))
               (⊦= a 1) (⊦= d '()) (⊦= aa 2) (⊦= dd 3)))

  ((test/λ _)
   (⊦= 5 ((λ (x) (+ x 2)) 3))
   (⊦= 5 ((λ (x) ((λ (y) (+ x y)) 2)) 3))
   (⊦= 5 ((λ (x y) (+ x y)) 2 3)))

  ((test/letmaptensor _)
   (⊦= '((((2 a #t) (2 a #t)) ((2 1 #t) (2 1 #f)))
           (((3 a #f) (3 a #t)) ((3 2 #f) (3 2 #f)))
           (((4 a #t) (4 a #t)) ((4 3 #t) (4 3 #f))))
         (letmaptensor ((x (list 1 2 3))
                        (y `(a ,x))
                        (z (list (odd? x) (symbol? y))))
                       (list (add1 x) y z))))

  ((test/letmap _)
   (⊦= '((2 a #t)
           (2 a #t)
           (2 1 #t)
           (2 1 #f)
           (3 a #f)
           (3 a #t)
           (3 2 #f)
           (3 2 #f)
           (4 a #t)
           (4 a #t)
           (4 3 #t)
           (4 3 #f))
         (letmap ((x (list 1 2 3))
                  (y `(a ,x))
                  (z (list (odd? x) (symbol? y))))
                 (list (add1 x) y z))))

  ((test/procc/λ-memo _)

   (define count 0)
   (define fib (λ (n) 
                   (add1! count)
                   (cond
                     ((< n 2) n) 
                     (else (let1 (m (sub1 n)) 
                                 (+ (fib m) (fib (sub1 m))))))))
   (let1 (v (fib 10))
         (⊦= 55 v)
         (⊦= 177 count))

   (set! count 0)

   (define fib-memo
     (λ-memo (n) 
              (add1! count)
              (cond
                ((< n 2) n) 
                (else (let1 (m (sub1 n)) 
                            (+ (fib-memo m) (fib-memo (sub1 m))))))))
   (let1 (v (fib-memo 10))
         (⊦= 55 v)
         (⊦= 11 count)))


  ((test/simdjson/load-twitter _)
   (define twitter-json (simdjson-load "twitter.json"))
   (⊦= 2 (length twitter-json))
   (⊦= '("search_metadata"
           (("completed_in" 0.087)
            ("max_id" 505874924095815700)
            ("max_id_str" "505874924095815681")
            ("next_results"
             "?max_id=505874847260352512&q=%E4%B8%80&count=100&include_entities=1")
            ("query" "%E4%B8%80")
            ("refresh_url"
             "?since_id=505874924095815681&q=%E4%B8%80&include_entities=1")
            ("count" 100)
            ("since_id" 0)
            ("since_id_str" "0"))) (cadr twitter-json))
   (⊦= 100 (vector-length (cadar twitter-json)))
   `(doc (p "Loaded a testbed json file " (code/inline "twitter.json") 
            (cite/a "https://raw.githubusercontent.com/simdjson/simdjson/refs/heads/master/jsonexamples/twitter.json" 
              (code/inline "twitter.json") " : a twitter search result JSON file")
            " with the " (code/inline "simdjson") 
            (cite/a "https://github.com/simdjson/simdjson" (code/inline "simdjson") " : parsing gigabytes of JSON per second")
            (cite/a "https://simdjson.github.io/simdjson/index.html" (code/inline "simdjson") " : user manual and documentation")
            " library. The file contains a twitter search result with 100 tweets. ")))

  ((test/simdjson/load-twitter/ondemand _)
   (⊦= (simdjson-load "twitter.json") (simdjson-load/ondemand "twitter.json"))
   `(doc (p "Here we show the " (i "on demand") " parsing mode of " (code/inline "simdjson") 
            (cite/a "https://simdjson.github.io/simdjson/md_doc_basics.html#documents-are-iterators" 
            (code/inline "simdjson") " : Documents are iterators") 
            ", which yields equivalent Scheme objects with respect to the DOM parsing.")))

            

  )

(unittest/✓ auxtest)














































