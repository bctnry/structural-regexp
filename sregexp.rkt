#lang racket

;;; structural regexp.
;;; a helper for writing complex regexp in racket.
;;; (c) Tset'ien Lin 2018

(provide sregexp->pregexp)

#|
grammar:
sregexp  ::= (literal regexp)
           | (raw regexp)
           | (concat sregexp ... sregexp)
           | (union sregexp ... sregexp)
           | (zero-or-more regexp)
           | (one-or-more regexp)
           | (zero-or-one regexp)
           | (zero-or-more-shortest regexp)
           | (one-or-more-shortest regexp)
           | (repeat-n regexp n)
           | (repeat-atleast regexp n)
           | (repeat-atmost regexp n)
           | (repeat-rng regexp n m)
           | (capture sregexp)
           | (expect-for string-literal)
           | (except-for string-literal)
           | (case-insensitive regexp)
           | (range start end)
           | 'linestart
           | 'lineend
|#

(define (sregexp->regexpstr srx)
  (define (escape str)
    (regexp-replace* #rx"\\\\|\\.|\\*|\\||\\?|\\[|\\]|\\(|\\)" str
                     (λ (all) (string-append "\\" all))))
  (define (escape-incl-set str)
    (let ((has-right-bracket? (regexp-match #rx"\\]" str))
          (has-hypen? (regexp-match #rx"-" str))
          (has-slash? (regexp-match #rx"\\\\" str)))
      (string-append
       "(?:"
       (if has-right-bracket? "(?:\\])|" "")
       (if has-hypen? "(?:-)|" "")
       (if has-slash? "(?:\\\\)|" "")
       "(?:[" (regexp-replace* #rx"(?:\\])|(?:-)" str "") "])"
       ")")))
  (define (escape-incl-set-not str)
    (let ((has-right-bracket? (regexp-match #rx"\\]" str))
          (has-hypen? (regexp-match #rx"-" str))
          (has-slash? (regexp-match #rx"\\\\" str)))
      (string-append
       "(?:[^"
       (if has-right-bracket? "]" "")
       (regexp-replace* #rx"\\\\" (regexp-replace* #rx"[]-]" str "") "")
       (if has-slash? "\\\\" "")
       (if has-hypen? "-" "")
       "])")))
  (cond
    ((string? srx) (sregexp->regexpstr `(literal ,srx)))
    ((symbol? srx)
     (case srx
       ((linestart) "^")
       ((lineend) "$")))
    (#t
     (case (car srx)
       ((literal)
        (string-append "(?:" (escape (cadr srx)) ")"))
       ((raw)
        (string-append "(?:" (cadr srx) ")"))
       ((concat)
        (string-append
         "(?:"
         (apply string-append
                (for/list ((i (cdr srx))) (sregexp->regexpstr i)))
         ")"))
       ((union)
        (string-append
         "(?:"
         (string-join (for/list ((i (cdr srx))) (sregexp->regexpstr i)) "|")
         ")"))
       ((capture)
        (string-append
         "(?:("
         (sregexp->regexpstr (cadr srx))
         "))"))
       ((zero-or-more)
        (string-append
         "(?:" (sregexp->regexpstr (cadr srx)) "*)"))
       ((one-or-more)
        (string-append
         "(?:" (sregexp->regexpstr (cadr srx)) "+)"))
       ((zero-or-one)
        (string-append
         "(?:" (sregexp->regexpstr (cadr srx)) "?)"))
       ((zero-or-more-shortest)
        (string-append
         "(?:" (sregexp->regexpstr (cadr srx)) "*?)"))
       ((one-or-more-shortest)
        (string-append
         "(?:" (sregexp->regexpstr (cadr srx)) "+?)"))
       ((repeat-n)
        (string-append
         "(?:" (sregexp->regexpstr (cadr srx)) "{" (number->string (caddr srx)) "})"))
       ((repeat-atleast)
        (string-append
         "(?:" (sregexp->regexpstr (cadr srx)) "{" (number->string (caddr srx)) ",})"))
       ((repeat-atmost)
        (string-append
         "(?:" (sregexp->regexpstr (cadr srx)) "{," (number->string (caddr srx)) "})"))
       ((repeat-rng)
        (string-append
         "(?:" (sregexp->regexpstr (cadr srx))
         "{" (number->string (caddr srx)) "," (number->string (cadddr srx)) "})"))
       ((expect-for)
        (string-append  ;; the character ] and - needs to be escaped
         "(?:" (escape-incl-set (cadr srx)) ")"))
       ((except-for)
        (string-append
         "(?:" (escape-incl-set-not (cadr srx)) ")"))
       ((case-insensitive)
        (string-append
         "(?i:" (sregexp->regexpstr (cadr srx)) ")"))
       ((case-sensitive)
        (string-append
         "(?-i:" (sregexp->regexpstr (cadr srx)) ")"))
       ((range)
        (string-append
         "(?:[" (cadr srx) "-" (caddr srx) "])"))
       (else
        srx)))))
(define (sregexp->pregexp str)
  (pregexp (sregexp->regexpstr str)))
