(ns alphabet-cipher.coder)

(defn to-keyword [c]
  (keyword (str c)))

(def a-z (mapv #(to-keyword (char %)) (range 97 123)))

(defn idx->str [x]
  (if (> x 25)
    (name (a-z (rem x 26)))
    (name (a-z x))))

(defn key->idx [k c]
  (.indexOf c k))

(defn ->enc [[key char] coll]
  (let [k (key->idx key coll)
        c (key->idx char coll)
        i (+ k c)]
    (if (> i 25)
      (idx->str (rem i 26))
      (idx->str i))))

(defn ->dec [[key char] coll]
  (let [k (key->idx key coll)
        c (key->idx char coll)]
    (idx->str  (+ c (- 26 k)))))

(defn rpt-keyword [k m]
  (reduce str (take (count m) (cycle k))))

(defn match-key-message [keyword message]
  (map #(vector (to-keyword %1) (to-keyword %2))
       (rpt-keyword keyword message) message))

(defn encode [keyword message]
  (reduce (fn [a b] (str a (->enc b a-z))) ""
          (match-key-message keyword message)))

(defn check [keyword test]
    (= keyword (rpt-keyword test keyword)))

(defn decode [keyword message]
  (let [m (match-key-message keyword message)]
    (reduce (fn [a b] (str a (->dec b a-z))) ""
            (match-key-message keyword message))))

(defn decipher [cipher message]
  (let [keystr (decode message cipher)
        perms (reductions #(str % %2) "" keystr)]
    (loop [h (first perms)
           t (rest perms)]
      (cond
        (check keystr h) h
        (empty? t) :not-found
        :else (recur (first t) (rest t))))))

