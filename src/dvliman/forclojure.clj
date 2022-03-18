(ns dvliman.forclojure
  (:require [clojure.string :as str]))

(defn generate-palindromes [end]
  (filter (fn [x]
            (and (= (str x) (str/join "" (reverse (str x))))
                 (>= x end))) (range)))

(defn create-palindrome [input b is-odd]
  (loop [n (if (= is-odd 1) (/ input b) input)
         palin input]
    (if (> n 0)
      (recur (/ n b) (+ (* palin b) (mod n b)))
      palin)))

(defn problem-153 [ss]
  (if (seq (reduce (fn [acc s]
                     (if (seq (clojure.set/intersection acc s))
                       (reduced #{})
                       (clojure.set/union acc s)))
                   #{} ss))
    true
    false))

(defn problem-156 [default ks]
  (zipmap ks (repeat (count ks) default)))

(defn problem-157 [coll]
  (map-indexed (fn [index item]
                 [item index]) coll))

(defn problem-158 [f]
  (fn [& args]
    (reduce #(%1 %2) f args)))

#_(defn -main
  [& args]
  (println "hello world"))
