;;; (aux pie locations) -- the source locations that Pie keeps in its terms.
;;;
;;; A port of locations.rkt of Pie, the language of "The Little Typer" by Daniel P. Friedman and David Thrane
;;; Christiansen (https://github.com/the-little-typer/pie, commit 2c89553). Pie is distributed under the
;;; GNU AGPL-3.0, and so is this file, which is derived from it.
;;;
;;; The file is ported whole, with the upstream names and exports (cf. locations.rkt:2-7). A location wraps
;;; the syntax object of (aux pie syntax) that a term was read from, together with a flag that says whether
;;; the typechecker should send information about it to the info hook (the tooltips of DrRacket, and the
;;; TODO reports of main). It is the opaque Precise-Loc of basics.rkt:24-29.
;;;
;;; Upstream's struct is opaque, while define-struct of (aux pie racket) makes a transparent one, so two
;;; locations with equal parts are equal? here and not in Racket. Pie never compares locations themselves,
;;; it compares the Srcloc lists of location->srcloc (the TODO case of alpha.rkt), so the difference cannot
;;; be observed. The mutators that define-struct also defines are not exported.

(module (aux pie locations)
  (location? syntax->location location->syntax location->srcloc location-for-info? not-for-info)

  (import scheme
          (chicken base)
          (chicken format)
          (aux base)
          (aux pie racket)
          (aux pie syntax))

  ;;; ==================== LOCATIONS ====================

  ;; location : Syntax Boolean -> Location, with the predicate location? and the accessors location-syntax
  ;; and location-for-info?, cf. locations.rkt:9
  (define-struct location syntax for-info?)

  ;; location->syntax : Location -> Syntax, the name under which upstream provides location-syntax,
  ;; cf. locations.rkt:4
  (define location->syntax location-syntax)

  ;; syntax->location : Syntax -> Location, a location whose information is sent, cf. locations.rkt:11
  (define (syntax->location stx)
    (location stx #t))

  ;; not-for-info : Location -> Location, the same place with its information not sent, cf. locations.rkt:14
  (define (not-for-info loc)
    (location (location-syntax loc) #f))

  ;; location->srcloc : Location -> Srcloc, the list (source line column position span) of basics.rkt:22.
  ;; The source is formatted with ~a, so a syntax object without a source gives "#f", cf. locations.rkt:17
  (define (location->srcloc loc)
    (let1 (stx (location-syntax loc))
      (list (format "~a" (syntax-source stx))
            (syntax-line stx)
            (syntax-column stx)
            (syntax-position stx)
            (syntax-span stx)))))
