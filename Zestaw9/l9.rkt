#lang slideshow
; Zadanie 2
(define (count-change val lst)
  ; definiujemy wektor od 0 do val
  (define vec (make-vector (+ 1 val)))
  ; ustawiamy pole 0 na 1, bo przeciez zero mozemy otrzymac tylko w jeden sposob
  (vector-set! vec 0 1)
  ; definiujemy funkcje, ktora pozwala nam na ustawienie odpowiedniej wartosci
  ; w danym polu wektora
  (define (v-set! idx)
    (vector-set! vec idx
                 (foldl + 0
                        (map (λ (e) (cond [(< (- idx e) 0) 0]
                                          [(> (- idx e) (vector-length vec)) 0]
                                          [else (vector-ref vec (- idx e))])) lst))))
  ; definiujemy funkcje, ktora bedzie ustawiala wartosc dla kolejnych pol w wektorze
  (define (count i)
    (if (eq? i val)
        (v-set! val)
        (begin (v-set! i)(count (+ 1 i)))))
  (count 1)
  (vector-ref vec val)
)
; wersja poprawiona
(define (c-change val lst )
  ; definiujemy wektor od 0 do val
  (define vec (make-vector (+ 1 val)))
  ; ustawiamy pole 0 na 1, bo zero mozna otrzymac tylko w jeden sposob
  (vector-set! vec 0 1)
  ; definicja funkcji loopujacej
  (define (loop-vector idx)
    (for/list ([i (in-range 1 (+ 1 val))])
      (define res (- i idx))
      (define current (vector-ref vec i))
      ( if (< res 0) 0 (vector-set! vec i (+ current (vector-ref vec res))))
    ))
  (for/list ([i lst])
    (loop-vector i)
   )
  (vector-ref vec val)
)

; Zadanie 3
(define (count x) (length (filter (λ (a) (not (pair? a))) x)))

; Zadanie 4

(define (mk-mobile left right) (cons left right))
(define (mk-branch length struct) (cons length struct))
(define (left-branch st) (car st))
(define (right-branch st) (cdr st))
(define (branch-length st) (car st))
(define (branch-struct st) (cdr st))
(define (br-weight st)
  (define bst (branch-struct st))
  (if (pair? bst)
      (if (eq? 1 (length bst)) (car bst) (m-weight bst)) bst))
(define (m-weight st) (+ (br-weight (left-branch st)) (br-weight (right-branch st))))
(define (br-balanced? st)
  (define bst (branch-struct st))
  (if (pair? bst)
      (if (eq? 1 (length bst)) true (m-balanced? bst))
      true))
(define (m-balanced? st)
  (define left (left-branch st))
  (define right (right-branch st))
  (define t (eq? (* (branch-length left) (br-weight left)) (* (branch-length right) (br-weight right))))
  (and t (br-balanced? left) (br-balanced? right)))

;rysowanie
(define (br-draw st el)
  (define bst (branch-struct st))
  (hc-append el
  (if (pair? bst)
      (if (eq? 1 (length bst)) (rectangle el el) (m-draw bst (+ el 2)))
      (rectangle el el))))
(define (m-draw st el)
  (hc-append el (vline 5 20)
  (br-draw (left-branch st) (+ el 2))
  (hline 5 20)
  (vline 5 20)
  (circle el)
  (vline 5 20)
  (hline 5 20)
  (br-draw (right-branch st) (+ el 2))
  (vline 5 20)))

(define mobi (mk-mobile '(1 2) '(2 3)))
(define left (mk-branch 2 mobi))
(define right (mk-branch 10 mobi))
(define nmobi (mk-mobile left right))
(define mobia (mk-mobile '(1 1) '(1 1)))
(define lefti (mk-branch 2 mobia))
(define amobi (mk-mobile lefti lefti))