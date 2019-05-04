#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                 ;;
;; Ciphertext Statistics                                                           ;;
;; =====================                                                           ;;
;;                                                                                 ;;
;; The module should provide a bunch of functions that do statistical analysis of  ;;
;; ciphertext. The output is almost always just an ordered list (counts are        ;;
;; omitted).                                                                       ;;
;;                                                                                 ;;
;; Fill in the body for the skeletons and do not change the arguments. You can     ;;
;; define as many functions as you require, there's no special credit for          ;;
;; implementing/using all of them.                                                 ;;
;;                                                                                 ;;
;; CAUTION:                                                                        ;;
;; 1. Be mindful that bi, tri and quadgrams do not cross word boundaries. Hence,   ;;
;; you must process each word separately.                                          ;;
;;                                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Analyses
(provide cipher-monograms
         cipher-bigrams
         cipher-unique-neighbourhood
         cipher-neighbourhood
         cipher-trigrams
         cipher-quadgrams
         cipher-common-words-single
         cipher-common-words-double
         cipher-common-words-triple
         cipher-common-words-quadruple
         cipher-common-initial-letters
         cipher-common-final-letters
         cipher-common-double-letters
         rle
         extract
         gen-big
         ;; any other functions of your design come below:

         ;; my-fundoo-analysis
         )

;; Takes ciphertext and produces a list of cipher chars sorted in decreasing
;; order of frequency.

; to find monograms sorted in decrreasing  frequency

(define (cipher-monograms ciphertext)
  (extract (sort (rle (sort (filter  (lambda (x) (and (>= (char->integer x) 97)
                                                                      (<= (char->integer x) 122))) (string->list ciphertext)) char>?))
        (lambda (x y) (> (cdr x) (cdr y))))))


(define (rle l)
   (foldr (lambda (x y) (if(null? y) (list (cons x 1)) (if (equal? (car (car y)) x) (cons (cons x (+ 1 (cdar y))) (cdr y))
                                                                         (cons (cons x 1) y)))) '() l))
(define (extract l)
  (foldr (lambda (x y) (cons (car x) y)) '() l))


(define (cipher-bigrams cipher-word-list)
  (extract (sort (rle (sort (gen-big cipher-word-list) string>?))
        (lambda (x y) (> (cdr x) (cdr y))))))
(define (gen-big l)
  (if(null? l) '()
     (append (gen-word (string->list (car l)) ) (gen-big (cdr l)))))
(define (gen-word l)
  (if (< (length l) 2) '()
      (cons (list->string (list (car l) (cadr l))) (gen-word (cdr l)))))



(define (cipher-unique-neighbourhood cipher-bigrams-list mode)
  (define l cipher-bigrams-list)
  (define (cnt val id)
     (define ans1 (foldr (lambda (z y)(define x (string->list z)) (cond [(equal? mode 'both) 
                                                  (if(or(equal? (car x)(integer->char val))
                                                        (equal? (cadr x) (integer->char val))) (+ 1 y)
                                                                                          y)]
              
                               [(equal? mode 'predecessor) (if (equal? (car x)(integer->char val)) (+ 1 y)
                                                               y)]
                                             
                               [(equal? mode 'successor) (if (equal? (cadr x)(integer->char val)) (+ 1 y)
                                                               y)])) 0 l))
    (if (= val 123) id
  
    (cnt (+ 1 val) (append id (list (cons (integer->char val) ans1))))))
  (sort (cnt 97 '()) (lambda (x y) (> (cdr x) (cdr y)))))

(define (cipher-neighbourhood cipher-word-list mode)
  (define l (sort (gen-big cipher-word-list) string>?))
  (define (cnt val id)
     (define ans1 (foldr (lambda (z y)(define x (string->list z)) (cond [(equal? mode 'both) 
                                                  (if(or(equal? (car x)(integer->char val))
                                                        (equal? (cadr x) (integer->char val))) (+ 1 y)
                                                                                          y)]
              
                               [(equal? mode 'predecessor) (if (equal? (car x)(integer->char val)) (+ 1 y)
                                                               y)]
                                             
                               [(equal? mode 'successor) (if (equal? (cadr x)(integer->char val)) (+ 1 y)
                                                               y)])) 0 l))
    (if (= val 123) id
  
    (cnt (+ 1 val) (append id (list (cons (integer->char val) ans1))))))
  (sort (cnt 97 '()) (lambda (x y) (> (cdr x) (cdr y)))))

(define (cipher-common-words-single cipher-word-list)
  (extract (rle (sort  (filter  (lambda (x) (= (string-length x) 1)) cipher-word-list) string>?))))
                               
                               
                                                 
                                                 
  
;; Takes the cipher-word-list and produces a list of 3-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-trigrams cipher-word-list)
  '())

;; Takes the cipher-word-list and produces a list of 4-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-quadgrams cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of single letter words, sorted
;; in decreasing order of frequency. Each element must be a string!

;; Takes the cipher word list and produces a list of double letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-double cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of triple letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-triple cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of four letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-quadruple cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear at the
;; start of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-initial-letters cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear at the
;; end of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-final-letters cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear as
;; consecutive double letters in some word, sorted in decreasing order of
;; frequency. Each element must thus be a char!
(define (cipher-common-double-letters cipher-word-list)
  '())
