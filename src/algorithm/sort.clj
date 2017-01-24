(ns algorithm.sort
  (:require [algorithm.tree :refer :all])
  (:import [algorithm.tree Node]))

(defn insertion-sort
  ([[y & ys :as coll] x]
   (cond
     (nil? y) [x]
     (<= x y) (concat [x y] ys)
     :else (lazy-cat [y] (insertion ys x))))
  ([coll]
   (reduce insertion [] coll)))

(defn selection-sort
  [coll]
  (let [indexes (vec (range (count coll)))
        min-key-from (fn [acc i] (apply min-key acc (subvec indexes i)))
        swap (fn [coll i j] (assoc coll i (coll j) j (coll i)))]
    (reduce
     (fn [acc i]
       (swap acc i (min-key-from acc i))) coll indexes)))

(defn merge-sort
  [coll]
  (let [count (count coll)
        split? (> count 1)
        at (/ count 2)
        [left right] (split-at at coll)
        mrg (fn [[x & xs :as left] [y & ys :as right] tmp]
              (if (and (not-empty left) (not-empty right))
                (if (<= x y)
                  (recur xs right (conj tmp x))
                  (recur left ys (conj tmp y)))
                (concat tmp left right)))]
    (if split?
      (vec (mrg (merge-sort left) (merge-sort right) []))
      coll)))

(defn quick-sort
  [coll]
  (let [pivot (last coll)
        pivot? (some? pivot)]
    (if pivot?
      (vec (lazy-cat (quick-sort (filter #(< % pivot) coll))
                     (filter #{pivot} coll)
                     (quick-sort (filter #(> % pivot) coll)))))))

(defn- bubble-step
  ([coll] (reduce bubble-step [] coll))
  ([coll x]
   (if-let [y (last coll)]
     (if (> y x)
       (conj (pop coll) x y)
       (conj coll x))
     [x])))

(defn bubble-sort
  [coll]
  (let [r (bubble-step coll)
        success (= r coll)]
    (if success
      r
      (recur r))))

(defn- swap [a i j]
  (assoc a i (nth a j) j (nth a i)))

(defn- sift
  [coll start end]
  (loop [coll coll root start child (inc (* 2 root))]
    (if (< (inc (* 2 root)) end)
      (let [ch (if (and (< child (dec end)) (< (nth coll child) (nth coll (inc child))))
                 (inc child)
                 child)]
        (if (< (nth coll root) (nth coll ch))
          (recur (swap coll root ch) ch (inc (* 2 ch)))
          coll))
      coll)))

(defn heap-sort
  [coll]
  (let [count (count coll)
        init (reduce (fn [c i] (sift c i count))
                     coll
                     (range (int (/ (- count 2) 2)) -1 -1))]
    (reduce (fn [c i] (sift (swap c 0 i) 0 i)) init (range (dec count) 0 -1))))


(defn tree-sort
  [[x & rest]]
  (.flatten (reduce (fn [tree e] (.insert tree e)) (Node. x nil nil) rest)))

(let [l [5 3 1 2 9 0]]
  (println "Insertion sort" (insertion-sort l))
  (println "Selection sort" (selection-sort l))
  (println "Merge sort" (merge-sort l))
  (println "Quick sort" (quick-sort l))
  (println "bubble sort" (bubble-sort l))
  (println "Heap sort" (heap-sort l))
  (println "Tree sort" (tree-sort l)))
