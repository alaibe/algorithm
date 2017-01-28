(ns algorithm.vector.bloom-filter)

(defn bf-hash
  [v]
  (->> (hash v)
       (str)
       (seq)
       (map str)
       (map read-string)
       (vec)))

(defn bf-indexes
  [h]
  (->> [(subvec h 1 4) (subvec h 4 7) (subvec h 7 10)]
       (map (fn [i] (clojure.string/join "" i)))
       (map read-string)))

(defn bf-contains?
  [bloom v]
  (->> (bf-hash v)
       (bf-indexes)
       (every? (fn [i] (true? (nth bloom i))))))

(defn bf-insert
  [bloom v]
  (->> (bf-hash v)
       (bf-indexes)
       (reduce (fn [a i] (assoc a i true)) bloom)))

(let [l [5 3 1 2 9]
      init (vec (make-array Boolean/TYPE 1000))
      bloom (reduce bf-insert init [5 2])]
  (println "list" l)
  (println "contains? 5" (bf-contains? bloom 5))
  (println "contains? 10" (bf-contains? bloom 10)))