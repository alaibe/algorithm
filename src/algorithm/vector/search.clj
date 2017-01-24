(ns algorithm.vector.search)

(defn sequential-search
  [coll v]
  (when (seq coll)
    (or (= v (first coll))
        (recur (next coll) v))))

(defn binary-search
  [coll v]
  (loop [low 0 high (dec (count coll))]
    (if (> low high)
      nil
      (let [middle (quot (+ low high) 2) middle_v (nth coll middle)]
        (cond
          (= middle_v v) true
          (< middle_v v) (recur (inc middle) high)
          (> middle_v v) (recur low (dec middle)))))))

(let [l [0 1 4 5 9 11 14 17]]
  (println "sequential-search found" (sequential-search l 4))
  (println "sequential-search not found" (sequential-search l 10))
  (println "binary-search found" (binary-search l 17))
  (println "binary-search not found" (binary-search l 10)))