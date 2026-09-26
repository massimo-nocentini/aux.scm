(import (only (chicken file) create-temporary-file delete-file)
        (only (chicken io) read-string)
        (only (chicken irregex) irregex-replace/all)
        (aux unittest) (aux base) (aux sxml))

; renders `tree` with `SXML->HTML->file!` and answers the markup it wrote, without the
; doctype and the newlines the serializer puts between elements.
(define (SXML->HTML->string tree)
  (let* ((filename (create-temporary-file))
         (html (string-append filename ".html")))
    (SXML->HTML->file! tree filename)
    (let1 (s (with-input-from-file html read-string))
      (delete-file filename)
      (delete-file html)
      (irregex-replace/all "\n" (substring s (string-length "<!doctype html>")) ""))))

(define-suite sxml-suite

  ((test/math-numbers _)
   ; every real is `rational?` in CHICKEN: integers and flonums are plain `mn`s,
   (⊦= "<math display=\"block\"><mn>3</mn></math>" (SXML->HTML->string '((displaymath (m 3)))))
   (⊦= "<math display=\"block\"><mn>3.14</mn></math>" (SXML->HTML->string '((displaymath (m 3.14)))))
   ; only an exact ratio is a fraction, whose two children are elements.
   (⊦= "<math display=\"block\"><mfrac><mn>1</mn><mn>2</mn></mfrac></math>"
       (SXML->HTML->string '((displaymath (m 1/2)))))
   (⊦= "<math display=\"block\"><mfrac><mn>1</mn><mn>2</mn></mfrac></math>"
       (SXML->HTML->string '((displaymath (frac 1 2)))))
   (⊦= "<math display=\"block\"><mfrac><mi>x</mi><mn>2</mn></mfrac></math>"
       (SXML->HTML->string '((displaymath (frac (m x) 2)))))
   (⊦= "<math display=\"block\"><mrow><mi>x</mi><mi>+</mi><mn>1</mn></mrow></math>"
       (SXML->HTML->string '((displaymath (m (x + 1)))))))

  )

(unittest/✓ sxml-suite)
