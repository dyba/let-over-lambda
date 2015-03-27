(ns let-over-lambda.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; let over lambda

(def counter
  ^{:doc "A simple counter object"}
  (let [x (atom 0)]
    (fn [] (swap! x inc))))

;; lambda over let over lambda

(defn make-counter
  "Creates a counter object. You can think of this as the Counter class."
  []
  (let [x (atom 0)]
    (fn [] (swap! x inc))))

;; Counter objects are easy and simple toys
;; how about a block scanner that searches for
;; words and reports back if it has found a
;; word from fragments?

(defn block-scanner
  [trigger-str]
  (let [trig-seq (seq trigger-str)
        curr-seq (atom trig-seq)]
    (fn [data-str]
      (doseq [c (seq data-str)]
        (if @curr-seq
          (swap! curr-seq #(if (= (first %) c)
                             (next %)
                             trig-seq))))
      (empty? @curr-seq))))

;; let's add extra functionality to our block scanner
;; we want to count the number of letters we read before
;; we matched the trigger word

;; TODO

;; Now if we want all our counters to count up or all
;; to count down, that would be the equivalent of a
;; class variable that dictates the direction of the
;; counter

;; let over lambda over let

(let [direction (atom :up)]
  (defn toggle-counter-direction
    []
    (swap! direction #(if (= :up %) :down :up)))
  
  (defn counter-class
    []
    (let [counter (atom 0)]
      (fn []
        (if (= @direction :up)
          (swap! counter inc)
          (swap! counter dec))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro unit-of-time
  [value unit]
  `(* ~value
      ~(case unit
         s 1
         m 60
         h 3600
         d 86400
         ms 1/1000
         us 1/1000000)))

;; Clojure has letfn instead of nlet

(defmacro nif
  "Numeric if. `expr` must evaluate to a number."
  [expr pos zero neg]
  `(cond
     (pos? ~expr) ~pos
     (zero? ~expr) ~zero
     :else ~neg))

;; This implementation of nif is buggy because we allow side effects to occur
;; just try out (nif (do (println "Side effects!") -10) 1 3 5)
;; => Side effects!
;;    Side effects!
;;    5
;; A better implementation would null those side effects:

(defmacro nif
  [expr pos zero neg]
  (let [e (gensym)]
    `(let [~e ~expr] ;; We evaluate `expr` only once here
       (cond
         (pos? ~e) ~pos
         (zero? ~e) ~zero
         :else ~neg))))

;; Now if we pass an expression that contains side effects, those side
;; effects only occur once rather than twice
;; (nif (do (println "Side effects!") -10) 1 3 5)
;; => Side effects!
;;    5

(comment ;; here is the macroexpansion of the nif above
  ;; (macroexpand '(nif x "I'm positive!" "Zilch" "I'm negative :("))
  (if (clojure.core/pos? x)
    "I'm positive!"
    (clojure.core/cond
      (clojure.core/zero? x) "Zilch"
      :else "I'm negative :(")))

(comment ;; interestingly, Common Lisp expands to the following
  ;; (macroexpand '(nif x "I'm positive!" "Zilch" "I'm negative :("))
  "I'm positive!")

(comment ;; using CL's macroexpand-1 gives a form that is much closer to
  ;; Clojure's macroexpand
  ;; TODO: show example here
  )
