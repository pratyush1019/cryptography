#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt"))

(provide dictionary-closure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Dictionary Closure                                                               ;;
;; ==================                                                               ;;
;;                                                                                  ;;
;; A choice of substitution can really trigger more substitutions by looking at the ;;
;; partially decrypted text - surely there will be some words which can be uniquely ;;
;; determined using the dictionary. It is prudent to concretize this "choice" as    ;;
;; this purely deterministic (involving absolutely no guess-work). In more          ;;
;; technical terms, this is called "maintaining arc-consistency" (look it up on     ;;
;; Wikipedia).                                                                      ;;
;;                                                                                  ;;
;; This function must utilise the dictionary and the cipher-word-list. Decrypt each ;;
;; word (`utils:decrypt`) and check if the dictionary has:                          ;;
;;                                                                                  ;;
;; 1. a unique completetion!                                                        ;;
;;    - Extend your key using the information with this match. Continue exploring   ;;
;;      the words under the extended key.                                           ;;
;; 2. many completions.                                                             ;;
;;    - Do nothing, just continue exploring the words under the same key. If none   ;;
;;      of the words fall into (1), return with the key that you have built so far. ;;
;; 3. no completions!                                                               ;;
;;    - Return `#f` (false), indicating that this partial key is wrong, and we must ;;
;;      revert to the original key.                                                 ;;
;;                                                                                  ;;
;; Returns either of:                                                               ;;
;; a. a (possibly) extended-key.                                                    ;;
;; b. `#f` if this key led to case (3) for some word.                               ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define (dictionary-closure key)
 ; key)


(define (spwords len dic)
  (if(null? dic) '()
     (if(= len (length (string->list (car dic)))) (append (list (string->list (car dic))) (spwords len (cdr dic)))
        (spwords len (cdr dic)))))

(define (dictionary-closure key)
  (let ([l  (utils:cipher-word-list-f (utils:decrypt key utils:ciphertext))])
  ;(displayln l)
  (define (test l)
    (if(null? l) key
       (if (all-upper? (car l))(if (member (car l) utils:dictionary) (test (cdr l)) #f) 
       (let ([res (try key (string->list (car l)) '() (spwords (string-length (car l)) utils:dictionary))])
         (cond [(not res) #f]
               [(null? res) (test (cdr l))]
               [else (dictionary-closure (utils:add-substitution res key))])))))
  (test l)))

(define (all-upper? str)
  (andmap (lambda (x) (char-upper-case? x)) (string->list str)))

(define (try key w sub lis)
 ; (displayln w)
  (if (null? lis)(if (null? sub) #f sub)
      (let ([res  (sptry key w '() (car lis))])
         (cond [(not res) (try key w sub (cdr lis))]
               [(null? res) '()]
               [else (if(null? sub)(try key w res (cdr lis)) '())]))))
(define (sptry key w subl tesw)
  (cond [(null? w) subl]
        [(< (char->integer (car w)) 97) (if(equal? (car w) (car tesw))(sptry key (cdr w) subl (cdr tesw)) #f)]
        [(and (utils:is-monoalphabetic? (list (cons (car tesw) (car w))) key) (lowcheck (cons (car tesw) (car w)) subl))
         (sptry (utils:add-substitution (list (cons (car tesw) (car w))) key) (cdr w) (cons (cons (car tesw) (car w)) subl) (cdr tesw))]
        [else #f]))

(define (lowcheck p l)
  (if(null? l) #t
     (if (equal? (cdr p) (cdar l)) (if(not (equal? (car p) (caar l))) #f (lowcheck p (cdr l)))
         (lowcheck p (cdr l)))))
(define em (make-list 26 #\_))       
        
  (define em3 (utils:add-substitution '((#\E . #\o) (#\T . #\e) (#\A . #\w) (#\I . #\q)) em))     



