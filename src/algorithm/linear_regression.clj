(ns algorithm.linear-regression)

(defn square [x] (* x x))

(defn mean
  [xs]
  (/ (reduce + xs) (count xs)))

(defn variance
  [xs]
  (let [a (mean (map square xs))
        b (square (mean xs))]
    (- a b)))

(defn covariance
  [xs ys]
  (let [a (mean (map * xs ys))
        b (mean xs)
        c (mean ys)]
    (- a (* b c))))

(defn slope
  [xs ys]
  (/ (covariance xs ys) (variance xs)))

(defn intercept
  [xs ys]
  (- (mean ys) (* (slope xs ys) (mean xs))))

(defn predict
  [xs ys x]
  (+ (* (slope xs ys) x) (intercept xs ys)))

(let [xs [1 2 3]
      ys [3 5 7]
      x 10]
  (println "XS" xs)
  (println "YS" ys)
  (println "Slope" (slope xs ys))
  (println "Intercept" (intercept xs ys))
  (println "X" x)
  (println "Predict" (predict xs ys x)))
