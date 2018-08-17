(ns scheme-in-clj.core)

; ---- common function in Clojure form ----
(defn car [s]
  (first s))

(defn cdr [s]
  (next s))

(defn cadr [s]
  (car (cdr s)))

(defn caddr [s]
  (car (cdr (cdr s))))

(defn caadr [s]
  (car (car (cdr s))))

(defn cdadr [s]
  (cdr (car (cdr s))))

(defn cddr [s]
  (cdr (cdr s)))

(defn cdddr [s]
  (cdr (cdr (cdr s))))

(defn cadddr [s]
  (car (cdddr s)))

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


; set!
(defn assignment? [exp]
  (tagged-list? exp 'set!))

(defn assignment-variable [exp]
  (cadr exp))

(defn assignment-value [exp]
  (caddr exp))

; lambda
(defn lambda? [exp]
  (tagged-list? exp 'lambda))

(defn lambda-parameters [exp]
  (cadr exp))

(defn lambda-body [exp]
  (cddr exp))

(defn make-lambda [parameters body]
  (cons 'lambda (cons parameters body)))


; definition
(defn definition? [exp]
  (tagged-list? exp 'define))

(defn definition-variable [exp]
  (if (seq? (cadr exp))
    (caadr exp)
    (cadr exp)))

(defn definition-value [exp]
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)    ; parameter
                 (cddr exp)))) ; body

; if
(defn if? [exp]
  (tagged-list? exp 'if))

(defn if-predicate [exp]
  (cadr exp))

(defn if-consequent [exp]
  (caddr exp))

(defn if-alternative [exp]
  (if (not (nil? (cdddr exp)))
    (cadddr exp)
    'false))

(defn make-if [predicate consequent alternative]
  (list 'if predicate consequent alternative))

; begin
(defn begin? [exp]
  (tagged-list? exp 'begin))

(defn begin-actions [exp]
  (cdr exp))

(defn last-exp? [s]
  (nil? (cdr s)))

(defn first-exp [s]
  (car s))

(defn rest-exp [s]
  (cdr s))

(defn make-begin [s]
  (cons 'begin s))

(defn sequence->exp [s]
  (cond
    (nil? s) s
    (last-exp? s) (first-exp s)
    :else (make-begin s)))

; application
(defn application? [exp]
  (seq? exp))

(defn operator [exp]
  (car exp))

(defn operands [exp]
  (cdr exp))

(defn no-operands? [ops]
  (nil? ops))

(defn first-operands [ops]
  (car ops))

(defn rest-operands [ops]
  (cdr ops))

; cond
(defn cond? [exp]
  (tagged-list? exp 'cond))

(defn cond-clauses [exp]
  (cdr exp))

(defn cond-predicate [clause]
  (car clause))

(defn cond-else-clause? [clause]
  (= 'else (cond-predicate clause)))

(defn cond-actions [clause]
  (cdr clause))

(defn expand-clauses [clauses]
  (if (nil? clauses)
    'false
    (let [fst (car clauses)
          rst (cdr clauses)]
      (if (cond-else-clause? fst)
        (if (nil? rst)
          (sequence->exp (cond-actions fst))
          (throw (Exception. "else clause is not last.")))
        (make-if (cond-predicate fst)
                 (sequence->exp (cond-actions fst))
                 (expand-clauses rst))))))

(defn cond->if [exp]
  (expand-clauses (cond-clauses exp)))

