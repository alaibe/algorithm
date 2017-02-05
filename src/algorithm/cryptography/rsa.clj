(ns algorithm.cryptography.rsa
  (:require [integer.prime :refer [miller-rabin-prime? power-mod]]))

;TODO: Add bits size

(defn big-int
  []
  (+ 1000 (rand-int 1000)))

(defn generate-prime
  []
  (loop [x (big-int)]
    (if (miller-rabin-prime? x)
      x
      (recur (big-int)))))

(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn encode
  [blocks public-key modulus]
  (map #(power-mod % public-key modulus) blocks))

(defn decode
  [encoded private-key modulus]
  (encode encoded, private-key, modulus))

(defn generate-key-pair
  []
  (let [p1 (generate-prime)
        p2 (generate-prime)
        n (* p1 p2)
        t (* (dec p1) (dec p2))
        e (loop [x 2]
            (if (= 1 (gcd x t))
              x
              (recur (inc x))))
        d (loop [x 1]
            (if (= 1 (mod (* x e) t))
              x
              (recur (inc x))))]
    [e, d n]))

(let [[public-key private-key modulus] (generate-key-pair)
      value [255]
      encoded (encode value public-key modulus)
      decoded (decode encoded private-key modulus)]
  (println "Public key, Private key, Modulus" [public-key private-key modulus])
  (println "Message" value)
  (println "Encoded" encoded)
  (println "Decoded" decoded))