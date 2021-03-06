(ns dvliman.forclojure)

;; last element: https://4clojure.oxal.org/#/problem/19
(defn problem-19 [coll]
  (nth coll (dec (count coll))))

;; reverse interleave: https://4clojure.oxal.org/#/problem/43
(defn problem-43 [coll n]
  (apply map list (partition n coll)))

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

;; pascal triangle: https://4clojure.oxal.org/#/problem/97
(defn problem-97 [x]
  (cond
    (= x 1) [1]
    (= x 2) [1 1]
    :else (loop [r 2
                 y [1 1]]
            (if (= r x)
              y
              (recur (inc r)
                     (flatten
                      [1 (map + y (rest y)) 1]))))))

;; product digits: https://4clojure.oxal.org/#/problem/99
(defn problem-99 [x y]
  (let [digits (defn digits [n base]
                 (if (zero? (quot n base))
                   [n]
                   (concat (digits (quot n base) base) [(mod n base)])))]
    (digits (* x y) 10)))

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

;; intoCamelCase: https://4clojure.oxal.org/#/problem/102
(defn problem-102 [s]
  (clojure.string/replace
   s #"-(.)"
   (fn [[a letter]]
     (prn a ":" letter)
     (.toUpperCase letter))))

(problem-102 "multi-word-key")
;; sequences of pronounciation: https://4clojure.oxal.org/#/problem/110
(defn problem-110 [xs]
  (lazy-seq
   (let [new (flatten (map #(apply vector (clojure.set/map-invert (frequencies %))) (partition-by identity xs)))]
     (concat [new] (problem-110 new)))))

(defn problem-110-cleaner [xs]
  (let [ys (mapcat (juxt count first) (partition-by identity xs))]
    (lazy-cat [ys] (problem-110-cleaner ys))))

;; the balance of n: https://4clojure.oxal.org/#/problem/115
(defn problem-115 [n]
  (let [digits (defn digits [d]
                 (if (zero? (quot d 10))
                   [d]
                   (concat (digits (quot d 10)) [(mod d 10)])))
        components (digits n)
        half (quot (count components) 2)
        left-half  (take half components)
        right-half (drop (if (even? (count components)) half (inc half)) components)]
    (= (reduce + left-half)
       (reduce + right-half))))

;; re-implement map: https://4clojure.oxal.org/#/problem/118
(defn problem-118 [f coll]
  (rest (reductions #(f %2) (cons identity coll))))

;; sum of square of digits: https://4clojure.oxal.org/#/problem/120
(defn problem-120 [coll]
  (let [digits (defn digits [d]
                 (if (zero? (quot d 10))
                   [d]
                   (concat (digits (quot d 10)) [(mod d 10)])))
        pred (fn [n]
               (< n (reduce + (map #(* % %) (digits n)))))]
    (count (filter pred coll))))

;; read a binary number: https://4clojure.oxal.org/#/problem/122
(defn problem-122 [binary]
  (int (reduce + (map-indexed (fn [index item]
                                (Math/floor (* (Math/pow 2 index)
                                               (Integer/parseInt (str item)))))
                              (reverse (seq binary))))))

;; through the Looking Class: https://4clojure.oxal.org/#/problem/126
(def problem-126 (let [x java.lang.Class]
                   (and (= (class x) x) x)))

;; recognize playing cards: https://4clojure.oxal.org/#/problem/128
(defn problem-128 [x]
  (let [suites        {\D :diamond \H :heart \C :club \S :spade}
        special-ranks {\J 9 \Q 10 \K 11 \A 12 \T 8}
        ranks (merge (zipmap (map char (range 2 11)) (range))
                     special-ranks)
        [suit rank] x]
    {:suit (get suites suit) :rank (get ranks rank)}))

;; sum some set subsets: https://4clojure.oxal.org/#/problem/131
(defn problem-131 [& coll]
  (let [powerset (fn powerset [coll]
                   (reduce (fn [acc x]
                             (concat acc (map #(conj % x) acc))) #{#{}} coll))]
    (not (empty? (apply clojure.set/intersection (map (fn [xss]
                                                        (set (map #(reduce + %) (remove empty? xss)))) (map powerset coll)))))))

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



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
