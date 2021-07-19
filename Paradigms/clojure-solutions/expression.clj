"constant, variable, add, subtract, multiply, divide"

(defn constant[c] (constantly c))
(defn variable [name] #(%1 name))


(defn op [f]
  (fn [& operands]
    (fn [vars] (apply f (mapv #(% vars) operands)))))
  
(def add (op +))
(def subtract (op -))
(def multiply (op *))
(def divide (op (fn [& a](reduce (fn [a b] (/ a (double b))) (first a) (rest a)))))
(def negate subtract)
(def sum add)
(def avg (op (fn [& x] (/ (apply +  x) (count x)))))


(def operations {'+ add, '- subtract, '* multiply, '/ divide, 'negate negate, 
		'sum sum, 'avg avg})


(defn parser [expr] (cond 
	(number? expr) (constant expr)
	(symbol? expr) (variable (str expr))
	:else (apply (operations (first expr)) (mapv parser (rest expr)))))
	
(defn parseFunction [expr] (parser (read-string expr)))




