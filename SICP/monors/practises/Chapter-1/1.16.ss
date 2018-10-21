;練習問題1.16
;; 結局粘ったけど解けなかった。(惜しいところまでは言っていた。n = 1の扱いが微妙だったので回答をみた。)
(define (fast-expt b n) (expt-iter b n 1))
(define (expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (expt-iter b
                                (/ n 2)
                                (* a b b)
                                ))
          (else      (expt-iter b 
                                (- n 1) 
                                (* b a))
          )))
(define (even? n) (= (remainder n 2) 0))
                         
(display (fast-expt 2 2))

;;回答
;; http://www.serendip.ws/archives/373
(define (fast-expt b n) (expt-iter b n 1))
(define (expt-iter b n a)
   (cond ((= n 0) a)
         ((even? n ) (expt-iter (* b b)
                                (/ n 2)
                                a))
         (else expt-iter b
                        (- n 1)
                        (* a b))
   )
)
;; 疑問点
;; 偶数でb^2にしてしまって問題ないのか？
;; 奇数の場合は必ず、偶数に変換されるので問題ないらしい。
