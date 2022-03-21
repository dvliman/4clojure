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

;; oscilrate: https://4clojure.oxal.org/#/problem/144
(defn problem-144 [x & fs]
  (reductions (fn [acc f]
                (f acc)) x (cycle fs)))

(defn problem-156 [default ks]
  (zipmap ks (repeat (count ks) default)))

(defn problem-157 [coll]
  (map-indexed (fn [index item]
                 [item index]) coll))

(defn problem-158 [f]
  (fn [& args]
    (reduce #(%1 %2) f args)))

(defn problem-161 [f]
  (fn [& args]
    (reduce #(%1 %2) f args)))


(defn problem-166 [< x y]
  (cond (< x y) :lt
        (< y x) :gt
        :else   :eq))

(defn problem-171 [xs]
  (let [xs (-> xs sort distinct)
        result (reduce (fn [[acc start llast] x]
                         (if (> (- x llast) 1)
                           [(conj acc [start llast]) x x]
                           [acc start x])) [[] (first xs) (first xs)] xs)]
    (if (or (nil? (second result)) (nil? (last result)))
      []
      (conj (first result) [(second result) (last result)]))))

(defn problem-177 [s]
  (empty? (let [to-keep #{\{ \} \( \) \[ \]}
                  candidates (filter #(contains? to-keep %) (seq s))]
              (reduce
               (fn [stack x]
                 (cond
                   (= x \{) (conj stack \{)
                   (= x \() (conj stack \()
                   (= x \[) (conj stack \[)

                   (and (= x \}) (= (peek stack) \{)) (pop stack)
                   (and (= x \)) (= (peek stack) \()) (pop stack)
                   (and (= x \]) (= (peek stack) \[)) (pop stack)
                   :else (reduced [:not-empty])))
               [] candidates))))

#_(defn -main
  [& args]
  (println "hello world"))
