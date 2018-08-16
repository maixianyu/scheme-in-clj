(ns scheme-in-clj.core)
; ---- common function in Clojure form ----
(defn car [s]
  (first s))

(defn cdr [s]
  (next s))

(defn cadr [s]
  (car (cdr s)))

; ---- representation of expression ----

; self-evaluating?
(defn self-evaluating? [exp]
  (cond
    (number? exp) exp
    (string? exp) exp
    :else false))

; variable?
(defn variable? [exp]
  (symbol? exp))

; quoted?
(defn tagged-list? [exp tag]
  (if (seq? exp)
    (= (car exp) tag)
    false))

(defn quoted? [exp]
  (tagged-list? exp 'quote))

(defn text-of-quotation [exp]
  (cadr exp))

(text-of-quotation (list 'quote 'a))
