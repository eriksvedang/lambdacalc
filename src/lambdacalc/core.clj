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

(define EMPTY ((PAIR TRUE) TRUE))
(define UNSHIFT (λl . (λx . ((PAIR FALSE) ((PAIR x) l)))))
(define EMPTY? LEFT)
(define FIRST (λl . (LEFT (RIGHT l))))
(define REST (λl . (RIGHT (RIGHT l))))
(define RANGE (Z (λf . (λm . (λn . (((IF ((LESS-OR-EQUAL? m) n))
                                     (λx . (((UNSHIFT ((f (INC m)) n)) m) x)))
                                    EMPTY))))))

(define FOLD (Z (λf . (λl . (λx . (λg . (((IF (IS-EMPTY? l))
                                          x)
                                         (λy . (((g (((f (REST l)) x) g)) (FIRST l)) y)))))))))

(define MAP (λk . (λf . (((FOLD k) EMPTY)
                         (λl . (λx . ((UNSHIFT l) (f x))))))))

(define TEN ((MULTIPLY FIVE) TWO))
(define B   TEN)
(define F   (INC B))
(define I   (INC F))
(define U   (INC I))
(define ZED (INC U))

(define FIZZ     ((UNSHIFT ((UNSHIFT ((UNSHIFT ((UNSHIFT EMPTY) ZED)) ZED)) I)) F))
(define BUZZ     ((UNSHIFT ((UNSHIFT ((UNSHIFT ((UNSHIFT EMPTY) ZED)) ZED)) U)) B))
(define FIZZBUZZ ((UNSHIFT ((UNSHIFT ((UNSHIFT ((UNSHIFT BUZZ) ZED)) ZED)) I)) F))

(define DIV (Z (λf . (λm . (λn . (((IF ((LESS-OR-EQUAL? n) m))
                                   (λx . ((INC ((f ((SUBTRACT m) n)) n)) x)))
                                  ZERO))))))

(define PUSH (λl . (λx . (((FOLD l) ((UNSHIFT EMPTY) x)) UNSHIFT))))

;; ~ inspection ~
(defn to-integer [f]
  ((f inc) 0))

(defn to-boolean [f]
  ((f 'true) 'false))

(defn to-array [f]
  (into []
        (if (to-boolean (EMPTY? f))
          ()
          (cons (FIRST f) (to-array (REST f))))))

(defn to-char [f]
  (nth "0123456789BFiuz" (to-integer f)))

(defn to-string [f]
  (clojure.string/join (map to-char (to-array f))))

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

(define L1 ((UNSHIFT ((UNSHIFT ((UNSHIFT EMPTY) THREE)) TWO)) ONE))

(to-integer (FIRST L1))
(to-integer (FIRST (REST L1)))
(to-integer (FIRST (REST (REST L1))))
(to-boolean (EMPTY? L1))
(to-boolean (EMPTY? EMPTY))

(map to-integer (to-array L1))

(map to-integer (to-array ((RANGE ONE) FIVE)))

(to-integer (((FOLD L1) ZERO) ADD))
(to-integer (((FOLD L1) TWO) MULTIPLY))

(map to-integer (to-array ((MAP L1) INC)))

(to-string FIZZ)
(to-string BUZZ)
(to-string FIZZBUZZ)

(to-integer ((DIV TEN) TWO))

(map to-integer (to-array ((PUSH L1) TEN)))
