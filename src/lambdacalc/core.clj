(ns lambdacalc.core)

;; ~ Implementation ~
(defn error [& s]
  (throw (Exception. (apply str s))))

(defn expand [x]
  (if (list? x)
    (if (= (first x) 'λ)
      (list 'fn (second x) (expand (nth x 2)))
      (apply list (map expand x)))
    x))

(defmacro define [name lambda]
  `(do
     (def ~name ~(expand lambda))
     (reset-meta! (var ~name) {:original-form (quote ~lambda)})
     (quote ~lambda)))

(defmacro pp [x]
  `(:original-form (meta (var ~x))))

(defn to-integer [f]
  ((f inc) 0))

(defn to-boolean [f]
  ((f 'true) 'false))

;; ~ Definitions ~
(define ZERO    (λ [f] (λ [x] x)))
(define ONE     (λ [f] (λ [x] (f x))))
(define TWO     (λ [f] (λ [x] (f (f x)))))
(define THREE   (λ [f] (λ [x] (f (f (f x))))))
(define FIVE    (λ [f] (λ [x] (f (f (f (f (f x))))))))
(define FIFTEEN (λ [f] (λ [x] (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f x))))))))))))))))))
;; (define HUNDRED (λ [f] (λ [x] (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f(f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f x)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(define TRUE (λ [x] (λ [y] x)))
(define FALSE (λ [x] (λ [y] y)))
(define IF (λ [x] x))

(define ZERO? (λ [n] ((n (λ [x] FALSE)) TRUE)))

;; ~ Examples ~
(pp ZERO)
(pp ONE)
(pp TWO)
(pp THREE)
(pp IF)

(to-integer ZERO)
(to-integer ONE)
(to-integer TWO)
(to-integer THREE)
(to-integer FIVE)
(to-integer FIFTEEN)
;;(to-integer HUNDRED)

(to-boolean TRUE)
(to-boolean FALSE)
(to-integer (((IF TRUE) ONE) TWO))
(to-integer (((IF FALSE) ONE) TWO))

(to-boolean (ZERO? ZERO))
(to-boolean (ZERO? ONE))

;;(pp (((IF TRUE) ONE) TWO))


