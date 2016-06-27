(ns lambdacalc.core)

;; ~ Implementation ~
(defn error [& s]
  (throw (Exception. (apply str s))))

(defn expand [x]
  (if (and (list? x) (not (empty? x)))
    (let [head (first x)]
      (if (and (symbol? head) (= (first (str head)) \λ))
        (list 'fn [(symbol (clojure.string/join (rest (str head))))] (expand (nth x 2)))
        (apply list (map expand x))))
    x))

(defmacro define [name lambda]
  `(do
     (def ~name ~(expand lambda))
     (reset-meta! (var ~name) {:original-form (quote ~lambda)})
     (quote ~lambda)))

(defn pp [x]
  (cond
    (symbol? x) (:original-form (meta (resolve x)))
    (list? x) (apply list (map pp x))
    :else x))

(defn to-integer [f]
  ((f inc) 0))

(defn to-boolean [f]
  ((f 'true) 'false))

;; ~ Definitions ~
(define ZERO    (λf . (λx . x)))
(define ONE     (λf . (λx . (f x))))
(define TWO     (λf . (λx . (f (f x)))))
(define THREE   (λf . (λx . (f (f (f x))))))
(define FIVE    (λf . (λx . (f (f (f (f (f x))))))))
(define FIFTEEN (λf . (λx . (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f x))))))))))))))))))
;; (define HUNDRED (λf . (λx . (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f(f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f x)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(define TRUE (λx . (λy . x)))
(define FALSE (λx . (λy . y)))
(define IF (λx . x))

(define ZERO? (λn . ((n (λx . FALSE)) TRUE)))

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

(pp '(((IF TRUE) ONE) TWO))


