#lang racket

(require 2htdp/image)

(define (triforce x)
  (let sierpinski ([n x])
    (if (zero? n)
        (triangle 2 'solid 'red)
        (let ([t (sierpinski (- n 1))])
          (freeze (above t (beside t t)))))))