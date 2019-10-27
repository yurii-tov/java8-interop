(ns java8-interop.core
  (:import (java.util.function Consumer Supplier)))


;; Lambdas


(defmacro consumer
  "Create a new Consumer object.
   Valid arguments are:
    - Binding vector and body.
      Example: (consumer [x]
                 (do-somethin-with-x x)
                 (do-other-thing x))
    - Named function.
      Example: (consumer println)"
  [& args]
  (let [[a1] args]
    (if (vector? a1)
      `(reify Consumer
         (accept [this ~(first a1)]
           (do ~@(rest args))))
      `(consumer [~'x] (~a1 ~'x)))))


(defmacro supplier
  "Create a new Supplier object.
   Valid arguments are:
    - Arbitrary list of forms.
      Example: (supplier (do-a)
                         (do-b)
                         (do-c))"
  [& body]
  `(reify Supplier
     (get [this]
       (do ~@body))))


;; Stream API


(defn stream-seq 
  "Convert java Stream to lazy seq"
  [stream]
  (let [i (. stream iterator)]
    ((fn lazy []
       (when (. i hasNext) 
         (cons (. i next) (lazy-seq (lazy))))))))
