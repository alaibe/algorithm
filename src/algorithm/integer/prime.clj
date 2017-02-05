(ns integer.prime)

(defn power-mod
  [b e m]
  (defn m* [p q] (mod (* p q) m))
  (loop [b b, e e, x 1]
    (if (zero? e)
      x
      (if (even? e)
        (recur (m* b b) (/ e 2) x)
        (recur (m* b b) (quot e 2) (m* b x))))))

(defn find-d-and-s
  [n]
  (loop [d (dec n) s 0]
    (if (odd? d)
      [d s]
      (recur (quot d 2) (inc s)))))

(defn random-numbers
  [n k]
  (take k (repeatedly #(+ (rand-int (- n 3)) 2))))

(defn single
  [a d s n]
  (let [x (power-mod a d n)]
    (if (or (= x 1) (= x (dec n)))
      true
      (loop [z (power-mod x 2 n ) r s]
        (cond
          (= z 1) false
          (= z (dec n)) true
          (= r 0) false
          :else (recur (power-mod z 2 n) (dec r)))))))

(defn miller-rabin-prime?
  ([n] (miller-rabin-prime? n (min 1000 (/ n 2))))
  ([n k]
   (let [[d s] (find-d-and-s n)]
     (every? #(single % d s n) (random-numbers n k)))))

(println "Primes beteen 3-1000:")
(doseq [q (range 3 1000)
        :when (miller-rabin-prime? q)]
  (println " " q))