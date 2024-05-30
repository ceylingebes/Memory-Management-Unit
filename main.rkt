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


(define (page args page_table page_size) ; 3.4 DONE
  (map (lambda (address)
         (define divided (divide_address_space address page_size))
         (define page_number (car divided))
         (define page_offset (cadr divided))
         (define frame_number (list-ref page_table (binary_to_decimal page_number)))
         (string-append frame_number page_offset))
       args))


; helper function to calculate factorial value of an int
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))


; h4elper function to transform degrees to radians
(define (degree_to_radian degree)
  (* degree (/ pi 180)))


(define (find_sin value num) ; 3.5 DONE
  (define (taylor-term n x) ; the term inside the sigma(sum) operatoe
    (* (/ (expt -1 n) (factorial (+ (* 2 n) 1))) (expt x (+ (* 2 n) 1))))
  (define (taylor-sum n x acc) ; recursive part that sums up the taylor terms
    (if (>= n num)
        acc
        (taylor-sum (+ n 1) x (+ acc (taylor-term n x)))))
  (taylor-sum 0 (degree_to_radian value) 0))
