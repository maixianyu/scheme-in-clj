(ns scheme-in-clj.lazy)

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
  (or (symbol? exp) (= true exp) (= false exp)))

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

(defn rest-exps [s]
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

; ---- environment ----
(defn enclosing-environment [env]
  (cdr env))

(defn first-frame [env]
  (car env))

(def the-empty-environment '())

(defn make-frame [variables values]
  (loop [map-rec {}
         var-tmp variables
         val-tmp values]
    (if (nil? var-tmp)
      (atom map-rec)
      (recur (assoc map-rec (keyword (str (car var-tmp))) (car val-tmp))
             (next var-tmp)
             (next val-tmp)))))

(defn frame-variables [frame]
  (keys @frame))

(defn frame-values [frame]
  (vals @frame))

(defn add-binding-to-frame! [variable value frame]
  (reset! frame (assoc @frame variable value)))

(defn extend-environment [variables values base-env]
  (if (= (count variables) (count values))
    (cons (make-frame variables values) base-env)
    (if (> (count variables) (count values))
      (throw (IllegalArgumentException. (str "Too many arguments supplied" ", " variables ", " (count variables) ", " values ", " (count values))))
      (throw (IllegalArgumentException. "Too many values supplied")))))


(defn lookup-variable-value [var env]
  (let [key-word (keyword (str var))]
    (cond
      (nil? env) (throw (Exception. (str "Unbound variable " var)))
      (nil? (get @(car env) key-word)) (lookup-variable-value var (cdr env))
      :else (get @(car env) key-word))))

(defn set-variable-value! [_var _val env]
  (let [key-word (keyword (str _var))]
    (cond
      (nil? env) (throw (Exception. (str "Unbound variable " _var)))
      (nil? (get @(car env) key-word)) (set-variable-value! _var _val (cdr env))
      :else (swap! (car env) #(assoc % key-word _val)))))

(defn define-variable! [_var _val env]
  (let [key-word (keyword (str _var))]
    (if (nil? (first-frame env))
      (throw (Exception. (str "Null env" env)))
      (swap! (first-frame env) #(assoc % key-word _val)))))


; ---- predicate ----
(defn true? [x]
  (= x true))

(defn false? [x]
  (= x false))

; ---- procedure ----
(defn make-procedure [parameters body env]
  (list 'procedure parameters body env))

(defn compound-procedure? [p]
  (tagged-list? p 'procedure))

(defn procedure-parameters [p]
  (cadr p))

(defn procedure-body [p]
  (caddr p))

(defn procedure-env [p]
  (cadddr p))

; primitive procedure
(defn primitive-procedure? [proc]
  (tagged-list? proc 'primitive))

(defn primitive-implementation [proc]
  (cadr proc))

(def primitive-procedures
  (list (list 'cons cons)
        (list 'car car)
        (list 'cdr cdr)
        (list 'map map)
        (list '= =)
        (list '+ +)
        (list '- -)
        (list '> >)
        (list '< <)
        (list 'null? nil?)))

(def primitive-procedure-names
  (map car primitive-procedures))

(def primitive-procedure-objects
  (map #(list 'primitive (cadr %))
       primitive-procedures))

(def setup-environment
  (let [initial-env (extend-environment primitive-procedure-names
                                        primitive-procedure-objects
                                        the-empty-environment)]
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(def the-global-environment setup-environment)

; apply-in-underlying-clojure
(def apply-in-underlying-clojure apply)
(def eval-in-underlying-clojure eval)

(defn apply-primitive-procedure [proc args]
  (apply-in-underlying-clojure (primitive-implementation proc)
                               args))

; procedure args
(declare eval)
(declare actual-value)

; thunk
(defn delay-it [exp env]
  (list 'thunk (atom exp) (atom env) (atom false)))

(defn thunk? [obj]
  (and (tagged-list? obj 'thunk)
       (false? @(cadddr obj))))

(defn thunk-exp [thunk]
  @(cadr thunk))

(defn thunk-env [thunk]
  @(caddr thunk))

(defn evaluated-thunk? [obj]
  (and (tagged-list? obj 'thunk)
       (true? @(cadddr obj))))

(defn thunk-value [evaluated-thunk]
  @(cadr evaluated-thunk))

(defn force-it [obj]
  (cond
    (thunk? obj) (let [result (actual-value (thunk-exp obj)
                                            (thunk-env obj))]
                   (reset! (cadddr obj) true)
                   (reset! (caddr obj) '())
                   (reset! (cadr obj) result)
                   result)
    (evaluated-thunk? obj) (thunk-value obj)
    :else obj))

; sub eval
(defn list-of-values [exps env]
  (if (no-operands? exps)
    '()
    (map #(actual-value % env) exps)))

(defn list-of-delayed-args [exps env]
  (if (no-operands? exps)
    '()
    (map #(delay-it % env) exps)))

(defn eval-if [exp env]
  (if (true? (actual-value (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(defn eval-sequence [exps env]
  (if (last-exp? exps)
    (eval (first-exp exps) env)
    (do
      (eval (first-exp exps) env)
      (eval-sequence (rest-exps exps) env))))

(defn eval-assignment [exp env]
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(defn eval-definition [exp env]
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(defn actual-value [exp env]
  (force-it (eval exp env)))

; apply
(defn apply [procedure arguments env]
  (cond
    (primitive-procedure? procedure) (apply-primitive-procedure procedure
                                                                (list-of-values arguments env))
    (compound-procedure? procedure) (eval-sequence (procedure-body procedure)
                                                   (extend-environment (procedure-parameters procedure)
                                                                       (list-of-delayed-args arguments env)
                                                                       (procedure-env procedure)))
    :else (throw (Exception. (str "Unknow procedure type -- APPLY" procedure)))))

; eval
(defn eval [exp env]
  (cond
    (tagged-list? exp 'debug) (print env)
    (self-evaluating? exp) exp
    (variable? exp) (lookup-variable-value exp env)
    (quoted? exp) (text-of-quotation exp)
    (assignment? exp) (eval-assignment exp env)
    (definition? exp) (eval-definition exp env)
    (if? exp) (eval-if exp env)
    (lambda? exp) (make-procedure (lambda-parameters exp)
                                  (lambda-body exp)
                                  env)
    (begin? exp) (eval-sequence (begin-actions exp) env)
    (cond? exp) (eval (cond->if exp) env)
    (application? exp) (apply (actual-value (operator exp) env)
                              (operands exp)
                              env)
    :else (throw (Exception. (str "Unknow expression type -- EVAL" exp)))))

; input and output
(def input-prompt ";;; M-Eval input:")
(def output-prompt ";;; M-Eval output:")

(defn prompt-for-input [string]
  (println string))

(defn announce-output [string]
  (print string))

(defn user-print [object]
  (if (compound-procedure? object)
    (print (list 'compound-procedure
                 (procedure-parameters object)
                 (procedure-body object)
                 '<procedure-env>))
    (do (print object) (newline))))


(defn driver-loop []
  (prompt-for-input input-prompt)
  (let [input (read)]
    (let [output (actual-value input the-global-environment)]
      (announce-output output-prompt)
      (user-print output)))
  (recur))


(driver-loop)
