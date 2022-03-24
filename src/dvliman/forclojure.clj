(ns dvliman.forclojure)

;; last element: https://4clojure.oxal.org/#/problem/19
(defn problem-19 [coll]
  (nth coll (dec (count coll))))

;; rotate sequence: https://4clojure.oxal.org/#/problem/44
(defn problem-44 [n xs]
  (let [len (count xs)
        r   (rem n len)
        x   (if (pos? r) r (+ len r))]
    (concat (drop x xs) (take x xs))))

;; intro to iterate: https://4clojure.oxal.org/#/problem/45
(defn problem-45 []
  (= '(1 4 7 10 13) (take 5 (iterate #(+ 3 %) 1))))

;; flipping out: https://4clojure.oxal.org/#/problem/46
(defn problem-46 [f]
  (fn [index coll]
    (f coll index)))

;; split by type: https://4clojure.oxal.org/#/problem/50
(defn problem-50 [coll]
  (vals (group-by type coll)))

;; reverse interleave: https://4clojure.oxal.org/#/problem/43
(defn problem-53 [xs n]
  (reduce (fn [acc x]
            (let [pos (mod (nth x 0) n)
                  l1  (nth acc pos)
                  l2  (concat l1 [(nth x 1)])]
              (assoc acc pos l2)))
          (repeat n '())
          (map-indexed list xs)))

(problem-53 [1 2 3 4 5 6] 2)

;; levenshtein distance: https://4clojure.oxal.org/#/problem/101
(defn problem-101 [x y]
  (with-local-vars
   [levenshtein
    (memoize (fn [x y i j]
               (if (zero? (min i j))
                 (max i j)
                 (min (+ 1 (levenshtein x y (dec i) j))
                      (+ 1 (levenshtein x y i (dec j)))
                      (+ (if (= (nth (seq x) (dec i))
                                (nth (seq y) (dec j)))
                           0 1) (levenshtein x y (dec i) (dec j)))))))]
    (levenshtein x y (count x) (count y))))

;; product digits: https://4clojure.oxal.org/#/problem/99
(defn problem-99 [x y]
  (let [digits (defn digits [n base]
                 (if (zero? (quot n base))
                   [n]
                   (concat (digits (quot n base) base) [(mod n base)])))]
    (digits (* x y) 10)))

;; intervals: https://4clojure.oxal.org/#/problem/132
(defn problem-132 [pred value coll]
  (let [a (first coll)
        b (second coll)]
    (lazy-seq
     (cond
       (and (number? a) (number? b))
       (if (pred a b)
         (concat [a value] (problem-132 pred value (rest coll)))
         (concat [a] (problem-132 pred value (rest coll))))

       (and (number? a) (nil? b))
       [a]

       :else []))))

;; digits and bases: https://4clojure.oxal.org/#/problem/137
(defn problem-137 [n base]
  (if (zero? (quot n base))
    [n]
    (concat (problem-137 (quot n base) base) [(mod n base)])))

;; dot product: https://4clojure.oxal.org/#/problem/143
(defn problem-143 [c1 c2]
  (reduce + (map * c1 c2)))

;; pairwise disjoint sets: https://4clojure.oxal.org/#/problem/153
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

;; map defaults: https://4clojure.oxal.org/#/problem/156
(defn problem-156 [default ks]
  (zipmap ks (repeat (count ks) default)))

;; indexing sequences: https://4clojure.oxal.org/#/problem/157
(defn problem-157 [coll]
  (map-indexed (fn [index item]
                 [item index]) coll))

;; decurry: https://4clojure.oxal.org/#/problem/158
(defn problem-158 [f]
  (fn [& args]
    (reduce #(%1 %2) f args)))

;; subset and superset: https://4clojure.oxal.org/#/problem/161
(defn problem-161 [f]
  (fn [& args]
    (reduce #(%1 %2) f args)))

;; comparisons: https://4clojure.oxal.org/#/problem/166
(defn problem-166 [< x y]
  (cond (< x y) :lt
        (< y x) :gt
        :else   :eq))

;; intervals: https://4clojure.oxal.org/#/problem/171
(defn problem-171 [xs]
  (let [xs (-> xs sort distinct)
        result (reduce (fn [[acc start llast] x]
                         (if (> (- x llast) 1)
                           [(conj acc [start llast]) x x]
                           [acc start x])) [[] (first xs) (first xs)] xs)]
    (if (or (nil? (second result)) (nil? (last result)))
      []
      (conj (first result) [(second result) (last result)]))))

;; balancing bracket: https://4clojure.oxal.org/#/problem/177
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
