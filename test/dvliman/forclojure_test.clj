(ns dvliman.forclojure-test
  (:require [clojure.test :refer :all]
            [clojure.set]
            [dvliman.forclojure :as src]))

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
