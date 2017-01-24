(ns algorithm.vector.bloom-filter)

(defn is?
  [atom v])

(defn insert
  [atom v])

(let [l [5 3 1 2 9 0]
      atom {}]
  (map insert atom l)
  (println "Entries" l)
  (println "find 5" (is? atom 5)
    (println "find 10" (is? atom 5))))