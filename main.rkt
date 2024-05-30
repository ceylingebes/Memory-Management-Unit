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


(define (divide_address_space num page_size) ; 3.3 DONE
  (define page_size_bytes (* page_size 1024)) ; convert KB to bytes
  (define offset_bits (integer-length (- page_size_bytes 1))) ; calculate the number of bits for offset
  (define address_length (string-length num))
  (define page_number_bits (- address_length offset_bits)) ; calculate the number of bits for page number
  (define page_number (substring num 0 page_number_bits))
  (define page_offset (substring num page_number_bits address_length))
  (list page_number page_offset))
