(ns levenshtein
  (:require [clojure.core.cache :as cache]))

(defn curry [f & args]
  (partial apply f))

;; https://en.wikipedia.org/wiki/Levenshtein_distance

(defn string-distance-1 [a b]
  (cond
    (empty? a) (count b)
    (empty? b) (count a)
    (= (first a) (first b)) (recur (rest a) (rest b))
    :else (inc
           (min
            (string-distance-1 (rest a) b)
            (string-distance-1 a (rest b))
            (string-distance-1 (rest a) (rest b))))))

(defn my-memoize [f]
  (let [cache (atom {})]
    (fn [& args]
      (if (contains? @cache args)
        (@cache args)
        (let [result (apply f args)]
          (swap! cache assoc args result)
          result)))))

(string-distance-1 "a" "a")
(string-distance-1 "kitten" "sitten")
(string-distance-1 "sitten" "sittin")
(string-distance-1 "add" "daddy")
(string-distance-1 "car" "carpet")

(let [C (atom (cache/fifo-cache-factory {}))]
  (defn string-distance-2 [a b]
    (cond
      (cache/has? @C [a b]) (cache/lookup @C [a b])
      (empty? a) (let [result (count b)]
                   (swap! C cache/miss [a b] result)
                   result)
      (empty? b) (let [result (count a)]
                   (swap! C cache/miss [a b] result)
                   result)
      (= (first a) (first b)) (let [ignore (string-distance-2 (rest a) (rest b))]
                                (swap! C cache/miss [a b] ignore)
                                ignore)
      :else (let [remove  (string-distance-2 (rest a) b)
                  add     (string-distance-2 a (rest b))
                  replace (string-distance-2 (rest a) (rest b))
                  result  (+ 0.5 (min remove add replace))]
              (swap! C cache/miss [a b] result)
              result)))
  (defn string-distance-2-chache [] @C)
  (defn string-distance-2-chache-reset [] (swap! C cache/fifo-cache-factory {})))

(let [args ["fhsadfa " "hfkdsh a"]]
  (string-distance-2-chache-reset)
  (let [r1 (time (apply string-distance-1 args))
        r2 (time (apply string-distance-2 args))]
    (assert (= r1 r2))))
