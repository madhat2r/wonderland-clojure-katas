(ns wonderland-number.finder)

(defn test-num [x]
  (let [t (for [y (range 2 7)]
            (* y x))
        s (map #(set (str %)) t)]
    (apply = s)))

(defn wonderland-number []
    (loop [r (range 100000 999999)]
        (cond
          (empty? r) nil
          (test-num (first r)) (first r)
          :else (recur (rest r)))))
