(ns algorithm.tree)

(definterface INode
  (insert [v])
  (flatten [])
  (show []))

(deftype Node
  [val
   ^:volatile-mutable ^INode left
   ^:volatile-mutable ^INode right]

  INode
  (insert [this v]
    (let [n (Node. v nil nil)]
      (cond
        (>= v val) (if right
                     (.insert right v)
                     (set! right n))
        (< v val) (if left
                    (.insert left v)
                    (set! left n))))

    this)

  (flatten [this]
    (lazy-cat
      (when left
        (.flatten left))
      (vector val)
      (when right
        (.flatten right)))))