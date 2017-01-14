(ns algorithm.graph.dijkstra)

(def ^:private inf Double/POSITIVE_INFINITY)

(defn new-costs
  [graph costs unvisited current]
  (let [current-cost (get costs current)]
    (reduce-kv
     (fn
       [c edge edge-value]
       (if (unvisited edge)
         (update-in c [edge] min (+ current-cost edge-value))
         c))
     costs
     (get graph current))))

(defn dijkstra
  ([graph from] (dijkstra graph from nil))
  ([graph from to]
   (loop [costs (assoc (zipmap (keys graph) (repeat inf)) from 0)
          current from
          unvisited (disj (apply hash-set (keys graph)) from)]
     (cond
       (= current to) (select-keys costs [to])
       (empty? unvisited) costs
       :else (let [new-costs (new-costs graph costs unvisited current)
                   new-current (apply min-key new-costs unvisited)
                   new-unvisited (disj unvisited new-current)]
               (recur new-costs new-current new-unvisited))))))

(def graph {:a {:b 8
                :c 2
                :d 5}
            :b {:a 8
                :d 2
                :f 13}
            :c {:a 2
                :d 2
                :e 5}
            :d {:a 5
                :b 2
                :c 2
                :f 6
                :g 3}
            :e {:c 5
                :d 1
                :g 1}
            :f {:b 13
                :d 6
                :g 2
                :h 3}
            :g {:d 3
                :e 1
                :f 2
                :h 6}
            :h {:f 3
                :g 6}})

(apply min-key {:a 0 :b 8} [:a :b])
(println "Input Graph" graph)
(println "Graph dijkstra" (dijkstra graph :a))
