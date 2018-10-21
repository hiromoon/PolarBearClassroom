; http://www.serendip.ws/archives/381
; 解けなかった。そのまま計算すればよかった。
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0 ) b)
        ((even? count) (fib-iter a
                                 b
                                 (+ (* p p ) (* q q))
                                 (+ (* 2 p q) (* q q))
                                 (/ count 2)))
        (else fib-iter (+ (* a q) (* b q) (* a p))
                       (+ (* a q) (* b p))   
                       p
                       q
                       (- count 1))))
