(ns dvliman.forclojure-test
  (:require [clojure.test :refer :all]
            [dvliman.forclojure :as src]))

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
