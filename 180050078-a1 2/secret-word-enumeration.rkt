#lang racket

;; You can require more modules of your choice.
;(require "dict-closure.rkt")
(require racket/string
         racket/list
         (prefix-in utils: "utils.rkt")
         (prefix-in  algo: "dict-closure.rkt")
         )

(provide secret-word-enumeration
         secret
         secret2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                           ;;
;; Secret Word Enumeration                                                                   ;;
;; =======================                                                                   ;;
;;                                                                                           ;;
;; This step exploits the fact that all keys begin with a secret word that is a              ;;
;; proper SIX-LETTER word from the English dictionary.                                       ;;
;;                                                                                           ;;
;; Given a partial key, we can query the dictionary for a list of 6 letter words             ;;
;; that can potentially begin the key.                                                       ;;
;; We can then filter out potential candidates whose keys do not agree with our partial key. ;;
;;                                                                                           ;;
;; It is possible that given a (wrong) partial key, we discover that none of these           ;;
;; potential candidates can agree with our partial key. This really implies a                ;;
;; mistake in the choice of substitution, since dictionary-closure is completely             ;;
;; deterministic (modulo any bugs in your implementation :)                                  ;;
;;                                                                                           ;;
;; Hence, this function must return either of:                                               ;;
;; a. `#f` if there are no consistent candidates for this key.                               ;;
;; b. the original key if there are multiple consistent candidates.                          ;;
;; c. the complete key if there's only one consistent candidate!                             ;;
;;                                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (spwords len dic)
  (if(null? dic) '()
     (if(= len (length (string->list (car dic)))) (append (list (string->list (car dic))) (spwords len (cdr dic)))
        (spwords len (cdr dic)))))

(define (zip l1 l2)
  (if (null? l1) '()
      (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))
(define (to n)
  (if(= 0 n) '()
     (append (to (- n 1) ) (list n))))

(define (slice l i j) (foldr (lambda (x y) (if (and (>= (cdr x) i) (<= (cdr x) j)) (cons (car x) y) y)) '()
                             (zip l (to (length l)))))

(define (secret-word-enumeration key-after-dictionary-closure)
  (define li (secret  key-after-dictionary-closure))
  (if(= 1 (length li)) (car li)
     (if (null? li) #f
        key-after-dictionary-closure))) 

(define (secret2 l id ans)
  (if(null? l) (cons ans id)
     (if(not (algo:dictionary-closure (car l))) (secret2 (cdr l) id ans)
        (begin (write-char #\@)(write-char #\@)(write-char #\@)(write-char #\+)(write-char #\space)(display id)
                           (write-char #\newline)(utils:show-key (car l)) 
                           (write-char #\@)(write-char #\@)(write-char #\@)(write-char #\-)(newline)
                           (secret2 (cdr l) (+ id 1) (cons (car l) ans))))))
  
(define (secret key-after-dictionary-closure);; Returns a key or false (#f)
  (define key key-after-dictionary-closure)
  (define l (slice key-after-dictionary-closure 1 6))
  (define lis (spwords 6 utils:dictionary))
   (let ([res (try key l '() lis)])
         (cond [(null? res) '()]
               [else res])))

(define (try key l sub lis)
  (if(null? lis) sub
     (if(and (match l (car lis)) (comp key (utils:encryption-key (list->string (car lis))) )) (try key l (append sub (list (utils:encryption-key (list->string (car lis))))) (cdr lis))
        (try key l sub (cdr lis)))))

(define (comp k1 k2)
  (andmap (lambda (x y) (if(equal? x #\_) #t (if (equal? x y) #t #f))) k1 k2))

(define (distinct l b)
  (if(null? b) #t
     (if (member (car b) l) #f
         (distinct (cons (car b) l) (cdr b)))))
(define (match a b)
  (if (not (distinct '() b)) #f
      (match2 a b)))

(define (match2 a b)
  (if(null? a) #t
     (if(equal? (car a) #\_) (match2 (cdr a) (cdr b))
        (if (not (equal? (car a) (integer->char (+ 32 (char->integer (car b)))))) #f
            (match2 (cdr a) (cdr b))))))
      
  
  
