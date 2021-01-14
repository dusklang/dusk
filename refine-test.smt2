; @pre(i8.MIN <= a && a <= i8.MAX - 5)
; fn add_random(a: i8): i8 { a + rand(5) }
(assert
    (forall ((a Int) (rand Int))
        (=> (and (<= (- 128) a) (<= a 122) (<= 0 rand) (<= rand 5))
            (and (<= (- 128) (+ a rand)) (<= (+ a rand) 127))
        )
    )
)
(check-sat)

(reset)

; @pre(i8.MIN <= a + b && a + b <= i8.MAX)
; fn add(a: i8, b: i8): i8 { a + b }
(assert
    (forall ((a Int) (b Int))
        (=> (and (<= (- 128) (+ a b)) (<= (+ a b) 127))
            (and (<= (- 128) (+ a b)) (<= (+ a b) 127))
        )
    )
)

(check-sat)