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
    (symbol? x) (let [lookup (:original-form (meta (resolve x)))]
                  (if (nil? lookup)
                    x
                    (pp lookup)))
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

(define TRUE  (λx . (λy . x)))
(define FALSE (λx . (λy . y)))
(define IF    (λx . x))

(define ZERO? (λn . ((n (λx . FALSE)) TRUE)))

(define PAIR  (λx . (λy . (λf . (((IF f) x) y)))))
(define LEFT  (λp . (p TRUE)))
(define RIGHT (λp . (p FALSE)))

(define INC   (λn . (λp . (λx . (p ((n p) x))))))
(define SLIDE (λp . ((PAIR (RIGHT p)) (INC (RIGHT p))))) ;; see p. 175
(define DEC   (λn . (LEFT ((n SLIDE) ((PAIR ZERO) ZERO)))))

(define ADD      (λm . (λn . ((n INC) m))))
(define SUBTRACT (λm . (λn . ((n DEC) m))))
(define MULTIPLY (λm . (λn . ((n (ADD m)) ZERO))))
(define POWER    (λm . (λn . ((n (MULTIPLY m)) ONE))))

(define LESS-OR-EQUAL? (λm . (λn . (ZERO? ((SUBTRACT m) n)))))

(define Z (λf . ((λx . (f (λy . ((x x) y))))
                 (λx . (f (λy . ((x x) y)))))))

(define MOD (Z (λf . (λm . (λn . (((IF ((LESS-OR-EQUAL? n) m))
                                   (λx . (((f ((SUBTRACT m) n)) n) x)))
                                  m))))))

(define HUNDRED ((MULTIPLY FIVE) ((MULTIPLY FIVE) ((MULTIPLY TWO) TWO))))

;; ~ Examples ~
(pp 'ZERO)
(pp 'ONE)
(pp 'TWO)
(pp 'THREE)
(pp 'HUNDRED)
(pp 'IF)

(to-integer ZERO)
(to-integer ONE)
(to-integer TWO)
(to-integer THREE)
(to-integer FIVE)
(to-integer FIFTEEN)
(to-integer HUNDRED)

(to-boolean TRUE)
(to-boolean FALSE)
(to-integer (((IF TRUE) ONE) TWO))
(to-integer (((IF FALSE) ONE) TWO))

(to-boolean (ZERO? ZERO))
(to-boolean (ZERO? ONE))

(pp '(((IF TRUE) ONE) TWO))

(to-integer (LEFT ((PAIR TWO) THREE)))
(to-integer (RIGHT ((PAIR TWO) THREE)))

(to-integer (INC (INC FIVE)))
(to-integer (DEC (DEC FIVE)))

(to-integer ((ADD FIVE) THREE))
(to-integer ((SUBTRACT FIVE) THREE))
(to-integer ((MULTIPLY FIVE) THREE))
(to-integer ((POWER FIVE) THREE))

(to-boolean ((LESS-OR-EQUAL? THREE) FIVE))
(to-boolean ((LESS-OR-EQUAL? THREE) THREE))
(to-boolean ((LESS-OR-EQUAL? FIVE) THREE))

(to-integer ((MOD FIVE) THREE))
(to-integer ((MOD ((MULTIPLY FIVE) FIVE)) ((MULTIPLY TWO) TWO)))


