(ns java8-interop.core
  (:import (java.util.function Consumer Supplier)))
        

;; Lambdas


(defmacro consumer [param-vector & body]
  (let [param (first param-vector)]
    `(reify Consumer
       (accept [this ~param]
         (do ~@body)))))


(defmacro supplier [& body]
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
