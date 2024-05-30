;huriye ceylin gebes
;2021400306
;compiling: yes
;complete: yes
#lang racket

(provide (all-defined-out))


(define (binary_to_decimal binary) ; 3.1 DONE
  (define binary-list (reverse (string->list binary))) 
  (define (char-to-decimal c)
    (if (char=? c #\1) 1 0))
  (foldl (lambda (digit acc)
           (+ acc (* (car digit) (expt 2 (cdr digit)))))
         0
         (map (lambda (x y) (cons (char-to-decimal x) y))
              binary-list
              (range 0 (length binary-list)))))


(define (relocator args limit base) ; 3.2 DONE
  (map (lambda (binary-address)
         (let ((decimal-address (binary_to_decimal binary-address)))
           (if (> decimal-address limit)
               -1
               (+ decimal-address base))))
       args))

