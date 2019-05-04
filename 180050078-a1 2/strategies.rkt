#lang racket
(require "list-comprehension.rkt")
;; You can require more modules of your choice.
(require racket/list
         (prefix-in utils: "utils.rkt")
         (prefix-in stats: "statistics.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                     ;;
;; Strategies                                                                          ;;
;; ==========                                                                          ;;
;; For the purpose of this assignment, just the `etai` strategy is expected, since     ;;
;; we have dictionary-closure and secret-word-enumeration to leap-frog to the right    ;;
;; key. This technique would fail for harder keys which are arbitrary permutations of  ;;
;; the alphabet. We will be forced to explore many more strategies (along with         ;;
;; dictionary-closure of course).                                                      ;;
;;                                                                                     ;;
;; Strategies to guess substitutions for the key using statistical information about   ;;
;; - the English language from utils.rkt                                               ;;
;; - the cipher text      from statistics.rkt                                          ;;
;;                                                                                     ;;
;; Follow the function signature as indicated below. Deviations will make it           ;;
;; impossible for automatic grading of your submission.                                ;;
;; Moreover, we do not expect your strategies to require any more/different            ;;
;; arguments. Note that you recieve the key as argument, so you can at the very        ;;
;; least ensure that all the substitutions are monoalphabetic wrt this key.            ;;
;;                                                                                     ;;
;; Signature:                                                                          ;;
;; ```                                                                                 ;;
;; (define (my-fundoo-strategy key)                                                    ;;
;;   ;; Make use of `utils:ciphertext`, `utils:cipher-word-list`                       ;;
;;   ...)                                                                              ;;
;; ```                                                                                 ;;
;;                                                                                     ;;
;; Substitutions                                                                       ;;
;; -------------                                                                       ;;
;; In order to extend the key incrementally, we use `utils:add-substitution` to        ;;
;; extend a given key with a substitution.                                             ;;
;;                                                                                     ;;
;; A substitution is a list of pairs, each pair mapping a plaintext char to a          ;;
;; ciphertext char. For example, to extend the key with T -> a and O -> r              ;;
;; (simultaneously), we use the substitution:                                          ;;
;; ```                                                                                 ;;
;; (list (cons #\T #\a) (cons #\O #\r))                                                ;;
;; ```                                                                                 ;;
;; For a single substitution use a singleton list (containing just one pair).          ;;
;;                                                                                     ;;
;; **CAUTION**                                                                         ;;
;; -----------                                                                         ;;
;; 1. Note that add-substitution does not do sanity checks on the substitution and use ;;
;;    of `utils:is-monoalphabetic` is recommended to ensure that you don't             ;;
;;    inadvertently create invalid keys.                                               ;;
;; 2. You must provide a list called `compositions` in this module.                    ;;
;;                                                                                     ;;
;; See docs in "utils.rkt" for more information.                                       ;;
;;                                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You must add "public" functions of this module to this list.
(provide etai
         other
         ;; Some more suggested strategies:
         
         ;; common-words-double
         ;; bigrams
         ;; common-initial-letters
         ;; common-final-letters
         ;; common-words-triple
         ;; trigrams
         ;; common-double-letters
         ;; common-words-quadruple
         ;; quadgrams
         
         ;; lists of strategies
         composition)

;; A strategy that uses some statistical information to generate potential
;; substitutions for E, T, A and I.
;; Refer the assignment manual for tips on developing this strategy. You can
;; interact with our etai with the executable we provide.
(define (zip l1 l2)
  (if (null? l1) '()
      (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))
(define (to n)
  (if(= 0 n) '()
     (append (to (- n 1) ) (list n))))

(define (slice l i j) (foldr (lambda (x y) (if (and (>= (cdr x) i) (<= (cdr x) j)) (cons (car x) y) y)) '()
                             (zip l (to (length l)))))

(define cipher-bigrams-list (stats:extract (sort (stats:rle (sort (stats:gen-big utils:cipher-word-list) string>?))
        (lambda (x y) (> (cdr x) (cdr y))))))

(define mono (map (lambda (x) (string-ref x 0)) (stats:cipher-common-words-single utils:cipher-word-list)))

(define topfe ( filter (lambda (x) (not (member x mono))) (slice (stats:cipher-monograms utils:ciphertext) 1 5)))
(define topft (reverse (map (lambda (x) (car x)) (filter (lambda (x) (member (car x) topfe))
                                                        (stats:cipher-unique-neighbourhood cipher-bigrams-list 'both)))))

(define (eta key)
  (if(= 2 (length mono))
  (lc (list (cons #\E a) (cons #\T b) (cons #\A c) (cons #\I d)) : a <- topfe b <- topft c <- mono d <- mono
      @ (not (equal? a b)) @ (not (equal? c d)))
  (if(= 1 (length mono))
     (append (lc (list (cons #\E a) (cons #\T b) (cons #\A c))  : a <- topfe b <- topft c <- mono 
      @ (not (equal? a b)) )
             (lc (list (cons #\E a) (cons #\T b)  (cons #\I d)) : a <- topfe b <- topft  d <- mono
      @ (not (equal? a b)) ))
     (lc (list (cons #\E a) (cons #\T b)) : a <- topfe b <- topft 
      @ (not (equal? a b)) ))))
(define (etai key)
  (filter (lambda (x) (utils:is-monoalphabetic? x key)) (eta key)))
     

(define (other key)
  (if(= 2 (length mono))
  (append (lc (list (cons #\E a)  (cons #\A c) (cons #\I d)) : a <- topfe c <- mono d <- mono
      @ (not (equal? c d)))
          (lc (list  (cons #\T b) (cons #\A c) (cons #\I d)) : b <- topft c <- mono d <- mono
      @ (not (equal? c d))))
  (if(= 1 (length mono))
     (append (lc (list (cons #\E a)  (cons #\A c))  : a <- topfe  c <- mono 
       )
             (lc (list (cons #\E a)  (cons #\I d)) : a <- topfe   d <- mono
       )
             (lc (list  (cons #\T b) (cons #\A c))  :  b <- topft c <- mono 
       )
             (lc (list  (cons #\T b)  (cons #\I d)) :  b <- topft  d <- mono
       ))
    (append  (lc (list  (cons #\T b)) :  b <- topft 
       )
             (lc (list (cons #\E a)) : a <- topfe  
       )))))

(define (brute key)
  (if(= 2 (length mono))
   (lc   (list (cons #\A c) (cons #\I d)) :  c <- mono d <- mono
      @ (not (equal? c d)))
          (if(= 1 (length mono))
     (append 
             (lc (list   (cons #\A c))  :   c <- mono 
       )
             (lc (list    (cons #\I d)) :    d <- mono
       ))
    (append  (lc (list  (cons #\T b)) :  b <- topft 
       )
             (lc (list (cons #\E a)) : a <- topfe  
       )))))

;; A suggested composition of strategies that might work well. Has not been
;; exhaustively tested by us. Be original ;)
(define composition (list etai other brute))
                  ;; common-words-double
                  ;; bigrams
                  ;; common-initial-letters
                  ;; common-final-letters
                  ;; common-words-triple
                  ;; trigrams
                  ;; common-double-letters))
                  ;; common-words-quadruple
                  ;; quadgrams))

