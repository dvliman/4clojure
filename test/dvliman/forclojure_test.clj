(ns dvliman.forclojure-test
  (:require [clojure.test :refer :all]
            [clojure.set]
            [dvliman.forclojure :as src]))

(deftest problem-19-last-element
  (is (= (src/problem-19 [1 2 3 4 5]) 5))
  (is (= (src/problem-19 '(5 4 3)) 3))
  (is (= (src/problem-19 ["b" "c" "d"]) "d")))

(deftest problem-43-reverse-interleave
  (is (= (src/problem-43 [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
  (is (= (src/problem-43 (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))
  (is (= (src/problem-43 (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))))

(deftest problem-44-rotate-sequence
  (is (= (src/problem-44 2 [1 2 3 4 5]) '(3 4 5 1 2)))
  (is (= (src/problem-44 -2 [1 2 3 4 5]) '(4 5 1 2 3)))
  (is (= (src/problem-44 6 [1 2 3 4 5]) '(2 3 4 5 1)))
  (is (= (src/problem-44 1 '(:a :b :c)) '(:b :c :a)))
  (is (= (src/problem-44 -4 '(:a :b :c)) '(:c :a :b))))

(deftest problem-46-flipping-out
  (is (= 3 ((src/problem-46 nth) 2 [1 2 3 4 5])))
  (is (= true ((src/problem-46 >) 7 8)))
  (is (= 4 ((src/problem-46 quot) 2 8)))
  (is (= [1 2 3] ((src/problem-46 take) [1 2 3 4 5] 3))))

(deftest problem-50-split-by-type
  (is (= (set (src/problem-50 [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}))
  (is (= (set (src/problem-50 [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]}))
  (is (= (set (src/problem-50 [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})))

(deftest problem-99-product-digits
  (is (= (src/problem-99 1 1) [1]))
  (is (= (src/problem-99 99 9) [8 9 1]))
  (is (= (src/problem-99 999 99) [9 8 9 0 1])))

(deftest problem-101-levenshtein-distance
  (is (src/problem-101 "kitten" "sitting"))
  (is (= (src/problem-101  "closure" "clojure") (src/problem-101 "clojure" "closure") 1))
  (is (= (src/problem-101  "xyx" "xyyyx") 2))
  (is (= (src/problem-101  "" "123456") 6))
  (is (= (src/problem-101 [1 2 3 4] [0 2 3 4 5]) 2))
  (is (= (src/problem-101 '(:a :b :c :d) '(:a :d)) 2))
  (is (= (src/problem-101 '"ttttattttctg" "tcaaccctaccat") 10))
  (is (= (src/problem-101 '"gaattctaatctc" "caaacaaaaaattt") 9)))

(deftest problem-122-read-a-binary-number
  (is (= 0 (src/problem-122 "0")))
  (is (= 7 (src/problem-122 "111")))
  (is (= 8 (src/problem-122 "1000")))
  (is (= 9 (src/problem-122 "1001")))
  (is (= 255 (src/problem-122 "11111111")))
  (is (= 1365 (src/problem-122 "10101010101")))
  (is (= 65535 (src/problem-122 "1111111111111111"))))

(deftest problem-128-recognize-playing-cards
  (is (src/problem-128 "H5"))
  (is (src/problem-128 "CA"))
  (is (src/problem-128 "DQ"))
  ;; pass in cljs but not clj
  #_(is (= (range 13) (map (comp :rank src/problem-128 str)
                        '[S2 S3 S4 S5 S6 S7
                          S8 S9 ST SJ SQ SK SA]))))


(deftest problem-132-intervals
  (is (= '(2) (src/problem-132 > :more [2])))
  (is (= [0 1 :x 2 :x 3 :x 4]  (src/problem-132 #(and (pos? %) (< % %2)) :x (range 5))))
  (is (empty? (src/problem-132 > :more ())))
  (is (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
                      (take 12 (->> [0 1]
                                    (iterate (fn [[a b]] [b (+ a b)]))
                                    (map first) ; fibonacci numbers
                                    (src/problem-132 (fn [a b] ; both even or both odd
                                          (= (mod a 2) (mod b 2)))
                                        :same)))))
  (is (= '(1 :less 6 :less 7 4 3) (src/problem-132 < :less [1 6 7 4 3]))))

(deftest problem-137-digits-and-bases
  (is (= [1 2 3 4 5 0 1] (src/problem-137 1234501 10)))
  (is (= [0] (src/problem-137 0 11)))
  (is (= [1 0 0 1] (src/problem-137 9 2)))
  (is (= [1 0] (let [n (rand-int 100000)] (src/problem-137 n n))))
  #_(is (= [22 6 10 5 0 19 6 9 6 31] (src/problem-137 js/Number.MAX_SAFE_INTEGER 42))))

(deftest problem-143-dot-product
  (is (= 0 (src/problem-143 [0 1 0] [1 0 0])))
  (is (= 3 (src/problem-143 [1 1 1] [1 1 1])))
  (is (= 32 (src/problem-143 [1 2 3] [4 5 6])))
  (is (= 256 (src/problem-143 [2 5 6] [100 10 1]))))

(deftest problem-144-oscilrate
  (is (= (take 3 (src/problem-144 3.14 int double)) [3.14 3 3.0]))
  (is (= (take 5 (src/problem-144 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7]))
  (is (= (take 12 (src/problem-144 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])))

(deftest problem-153-pairwise-disjoint-sets
    (is (= (src/problem-153 #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
           true))
    (is (= (src/problem-153 #{#{:a :b :c :d :e}
                             #{:a :b :c :d}
                             #{:a :b :c}
                             #{:a :b}
                             #{:a}})
           false))
    (is (= (src/problem-153 #{#{[1 2 3] [4 5]}
                             #{[1 2] [3 4 5]}
                             #{[1] [2] 3 4 5}
                             #{1 2 [3 4] [5]}})
           true))
    (is (= (src/problem-153 #{#{'a 'b}
                             #{'c 'd 'e}
                             #{'f 'g 'h 'i}
                             #{''a ''c ''f}})
           true))
    (is (= (src/problem-153 #{#{'(:x :y :z) '(:x :y) '(:z) '()}
                             #{#{:x :y :z} #{:x :y} #{:z} #{}}
                             #{'[:x :y :z] [:x :y] [:z] [] {}}})
           false))
    (is (= (src/problem-153 #{#{(= "true") false}
                             #{:yes :no}
                             #{(class 1) 0}
                             #{(symbol "true") 'false}
                             #{(keyword "yes") ::no}
                             #{(class '1) (int \0)}})
           false))
    (is (= (src/problem-153 (set [(set [distinct?])
                                 (set [#(-> %) #(-> %)])
                                 (set [#(-> %) #(-> %) #(-> %)])
                                 (set [#(-> %) #(-> %) #(-> %)])]))
           true))
    (is (= (src/problem-153 #{#{(#(-> *)) + (quote mapcat) #_ nil}
                             #{'+ '* mapcat (comment mapcat)}
                             #{(do) set contains? nil?}
                             #{, , , #_, , empty?}})
                       false)))

(deftest problem-156-map-defaults
  (is (= (src/problem-156 0 [:a :b :c]) {:a 0 :b 0 :c 0}))
  (is (= (src/problem-156 "x" [1 2 3]) {1 "x" 2 "x" 3 "x"}))
  (is (= (src/problem-156 [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})))

(deftest problem-157-indexing-sequences
  (is (= (src/problem-157 [:a :b :c]) [[:a 0] [:b 1] [:c 2]]))
  (is (= (src/problem-157 [0 1 3]) '((0 0) (1 1) (3 2))))
  (is (= (src/problem-157 [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]])))

(deftest problem-158-decurry
  (is (= 10 ((src/problem-158 (fn [a]
                                (fn [b]
                                  (fn [c]
                                    (fn [d]
                                      (+ a b c d))))))
             1 2 3 4)))
  (is (= 24 ((src/problem-158 (fn [a]
                                (fn [b]
                                  (fn [c]
                                    (fn [d]
                                      (* a b c d))))))
             1 2 3 4)))
  (is (= 25 ((src/problem-158 (fn [a]
                                (fn [b]
                                  (* a b))))
                          5 5))))

(deftest problem-161-subset-and-superset
  (is (clojure.set/superset? #{1 2} #{2}))
  (is (clojure.set/subset? #{1} #{1 2}))
  (is (clojure.set/superset? #{1 2} #{1 2}))
  (is (clojure.set/subset? #{1 2} #{1 2})))

(deftest problem-166-comparisons
  (is (= :gt (src/problem-166 < 5 1)))
  (is (= :eq (src/problem-166 (fn [x y] (< (count x) (count y))) "pear" "plum")))
  (is (= :lt (src/problem-166 (fn [x y] (< (mod x 5) (mod y 5))) 21 3)))
  (is (= :gt (src/problem-166 > 0 2))))

(deftest problem-171-intervals
  (is (= (src/problem-171 [1 2 3]) [[1 3]]))
  (is (= (src/problem-171 [10 9 8 1 2 3]) [[1 3] [8 10]]))
  (is (= (src/problem-171 [1 1 1 1 1 1 1]) [[1 1]]))
  (is (= (src/problem-171 []) []))
  (is (= (src/problem-171 [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])
                      [[1 4] [6 6] [9 11] [13 17] [19 19]])))

(deftest problem-177-balancing-brackets
  (is (src/problem-177 "This string has no brackets."))
  (is (src/problem-177 "class Test {
                          public static void main(String[] args) {
                            System.out.println(\"Hello world.\");
                          }
                        }"))
  (is (not (src/problem-177 "(start, end]")))
  (is (not (src/problem-177 "())")))
  (is (not (src/problem-177 "[ { ] } ")))
  (is (src/problem-177 "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))"))
  (is (not (src/problem-177 "([]([(()){()}(()(()))(([[]]({}([)))())]((((()()))))))")))
  (is (not (src/problem-177 "["))))
