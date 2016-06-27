(ns lambdacalc.core)

;; ~ Implementation ~
(defn error [& s]
  (throw (Exception. (apply str s))))

(defn expand [x]
  (if (and (list? x) (= (first x) 'λ))
    (list 'fn (second x) (expand (nth x 2)))
    x))

(defmacro define [name lambda]
  `(do
     (def ~name ~(expand lambda))
     (reset-meta! (var ~name) {:original-form (quote ~lambda)})
     (quote ~lambda)))

(defmacro pp [x]
  `(:original-form (meta (var ~x))))

(defn to-integer [f]
  ((f (fn [n] (inc n))) 0))

;; ~ Definitions ~
(define zero  (λ [x] x))
(define one   (λ [p] (λ [x] (p x))))
(define two   (λ [p] (λ [x] (p (p x)))))
(define three (λ [p] (λ [x] (p (p (p x))))))

;; ~ Examples ~
(pp zero)
(pp one)
(pp two)
(pp three)

(to-integer zero)
(to-integer one)
(to-integer two)
(to-integer three)
