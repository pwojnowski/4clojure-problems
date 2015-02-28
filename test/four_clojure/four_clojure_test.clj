(ns four-clojure.four_clojure-test
  (:require [clojure.test :refer :all]
            [four-clojure.four_clojure :refer :all]))

(deftest myflat-test
  (is (= (myflat '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))
  (is (= (myflat ["a" ["b"] "c"]) '("a" "b" "c")))
  (is (= (myflat '((((:a))))) '(:a))))

(deftest reverse-interleave-test
  (is (= (reverse-interleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
  (is (= (reverse-interleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))
  (is (= (reverse-interleave (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))))

(deftest rotate-sequence-test
  (is (= (rotate-sequence 2 [1 2 3 4 5]) '(3 4 5 1 2)))
  (is (= (rotate-sequence -2 [1 2 3 4 5]) '(4 5 1 2 3)))
  (is (= (rotate-sequence 6 [1 2 3 4 5]) '(2 3 4 5 1)))
  (is (= (rotate-sequence 1 '(:a :b :c)) '(:b :c :a)))
  (is (= (rotate-sequence -4 '(:a :b :c)) '(:c :a :b))))

(deftest rotate-sequence2-test
  (is (= (rotate-sequence2 2 [1 2 3 4 5]) '(3 4 5 1 2)))
  (is (= (rotate-sequence2 -2 [1 2 3 4 5]) '(4 5 1 2 3)))
  (is (= (rotate-sequence2 6 [1 2 3 4 5]) '(2 3 4 5 1)))
  (is (= (rotate-sequence2 1 '(:a :b :c)) '(:b :c :a)))
  (is (= (rotate-sequence2 -4 '(:a :b :c)) '(:c :a :b))))

(deftest flip-out-test
  (is (= 3 ((flip-out nth) 2 [1 2 3 4 5])))
  (is (= true ((flip-out >) 7 8)))
  (is (= 4 ((flip-out quot) 2 8)))
  (is (= [1 2 3] ((flip-out take) [1 2 3 4 5] 3))))

(deftest split-by-type-test
  (is (= (set (split-by-type [1 :a 2 :b 3 :c]))
         #{[1 2 3] [:a :b :c]}))
  (is (= (set (split-by-type [:a "foo"  "bar" :b]))
         #{[:a :b] ["foo" "bar"]}))
  (is (= (set (split-by-type [[1 2] :a [3 4] 5 6 :b]))
         #{[[1 2] [3 4]] [:a :b] [5 6]})))

(deftest mypartition-test
  (is (= (mypartition 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8))))
  (is (= (mypartition 2 (range 8)) '((0 1) (2 3) (4 5) (6 7))))
  (is (= (mypartition 3 (range 8)) '((0 1 2) (3 4 5)))))

(deftest count-occurs-test
  (is (= (count-occurs [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1}))
  (is (= (count-occurs [:b :a :b :a :b]) {:a 2, :b 3}))
  (is (= (count-occurs '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})))

(deftest distinct-items-test
  (is (= (distinct-items [1 2 1 3 1 2 4]) [1 2 3 4]))
  (is (= (distinct-items [:a :a :b :b :c :c]) [:a :b :c]))
  (is (= (distinct-items '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3])))
  (is (= (distinct-items (range 50)) (range 50))))

(deftest fun-comp-test
  (is (= [3 2 1] ((fun-comp rest reverse) [1 2 3 4])))
  (is (= 5 ((fun-comp (partial + 3) second) [1 2 3 4])))
  (is (= true ((fun-comp zero? #(mod % 8) +) 3 5 7 9)))
  (is (= "HELLO" ((fun-comp #(.toUpperCase %) #(apply str %) take) 5 "hello world"))))

(deftest juxtaposition-test
  (is (= [21 6 1] ((juxtaposition + max min) 2 3 5 1 6 4)))
  (is (= ["HELLO" 5] ((juxtaposition #(.toUpperCase %) count) "hello")))
  (is (= [2 6 4] ((juxtaposition :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))))

(deftest myiterate-test
  (is (= (take 5 (myiterate #(* 2 %) 1)) [1 2 4 8 16]))
  (is (= (take 100 (myiterate inc 0)) (take 100 (range))))
  (is (= (take 9 (myiterate #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))))


(deftest problem-65-black-box-testing-test
  ;; (is (= :map (problem-65 {:a 1, :b 2})))
  ;; (is (= :list (problem-65 (range (rand-int 20)))))
  ;; (is (= :vector (problem-65 [1 2 3 4 5 6])))
  ;; (is (= :set (problem-65 #{10 (rand-int 5)})))
  ;; (is (= [:map :set :vector :list] (map problem-65 [{} #{} [] ()])))
  )

(deftest prime-numbers-test
  (is (= (prime-numbers 2) [2 3]))
  (is (= (prime-numbers 5) [2 3 5 7 11]))
  (is (= (last (prime-numbers 100)) 541)))

(deftest word-sorting-test
  (is (= (word-sorting  "Have a nice day.")
         ["a" "day" "Have" "nice"]))
  (is (= (word-sorting  "Clojure is a fun language!")
         ["a" "Clojure" "fun" "is" "language"]))
  (is (= (word-sorting  "Fools fall for foolish follies.")
         ["fall" "follies" "foolish" "Fools" "for"])))

(deftest filter-perfect-squares-test
  (is (= (filter-perfect-squares "4,5,6,7,8,9") "4,9"))
  (is (= (filter-perfect-squares "15,16,25,36,37") "16,25,36")))

(deftest my-trampoline-test
  (is (= [1 3 5 7 9 11]
         (letfn
             [(foo [x y] #(bar (conj x y) y))
              (bar [x y] (if (> (last x) 10)
                           x
                           #(foo x (+ 2 y))))]
           (trampoline foo [] 1)))))

(deftest anagram-finder-test
  (is (= (anagram-finder ["meat" "mat" "team" "mate" "eat"])
         #{#{"meat" "team" "mate"}}))
  (is (= (anagram-finder ["veer" "lake" "item" "kale" "mite" "ever"])
         #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})))

(deftest problem-78-reimplement-trampoline-test
  (is (= (letfn [(triple [x] #(sub-two (* 3 x)))
                 (sub-two [x] #(stop?(- x 2)))
                 (stop? [x] (if (> x 50) x #(triple x)))]
           (my-trampoline triple 2))
         82))
  (is (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
                 (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
           (map (partial my-trampoline my-even?) (range 6)))
         [true false true false true false])))

(deftest perfect-number-test
  ;; (is (= (perfect-number? 6) true))
  ;; (is (= (perfect-number? 7) false))
  ;; (is (= (perfect-number? 496) true))
  ;; (is (= (perfect-number? 500) false))
  ;; (is (= (perfect-number? 8128) true))
  )

(deftest happy-numbers-test
  (are [x result] (= (happy-number? x) result)
       7 true
       986543210 true
       2 false
       3 false))

(deftest test-bintree
  (are [aseq result] (= (bintree? aseq) result)
       '(:a (:b nil nil) nil) true
       '(:a (:b nil nil)) false
       [1 nil [2 [3 nil nil] [4 nil nil]]] true
       [1 [2 nil nil] [3 nil nil] [4 nil nil]] false
       [1 [2 [3 [4 nil nil] nil] nil] nil] true
       [1 [2 [3 [4 false nil] nil] nil] nil] false
       '(:a nil ()) false))

(deftest symetric-bintree?-test
  (is (= (symetric-bintree? '(:a (:b nil nil) (:b nil nil))) true))
  (is (= (symetric-bintree? '(:a (:b nil nil) nil)) false))
  (is (= (symetric-bintree? '(:a (:b nil nil) (:c nil nil))) false))
  (is (= (symetric-bintree? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                             [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
         true))
  (is (= (symetric-bintree? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                             [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
         false))
  (is (= (symetric-bintree? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                             [2 [3 nil [4 [6 nil nil] nil]] nil]])
         false)))

(deftest test-pascal-nth
  (is (= (pascal-nth 1) [1]))
  (is (= (map pascal-nth (range 1 6))
         [     [1]
              [1 1]
             [1 2 1]
            [1 3 3 1]
           [1 4 6 4 1]]))
  (is (= (pascal-nth 11)
         [1 10 45 120 210 252 210 120 45 10 1])))

(deftest into-camel-case-test
  (is (= (into-camel-case "something") "something"))
  (is (= (into-camel-case "multi-word-key") "multiWordKey"))
  (is (= (into-camel-case "leaveMeAlone") "leaveMeAlone")))

(deftest pronunciations-test
  ;; (is (= [[1 1] [2 1] [1 2 1 1]] (take 3 (pronunciation [1]))))
  ;; (is (= [3 1 2 4] (first (pronunciation [1 1 1 4 4]))))
  ;; (is (= [1 1 1 3 2 1 3 2 1 1] (nth (pronunciation [1]) 6)))
  ;; (is (= 338 (count (nth (pronunciation [3 2]) 15))))
  )

(deftest mymap-test
  (is (= [3 4 5 6 7]
         (mymap inc [2 3 4 5 6])))
  (is (= (repeat 10 nil)
         (mymap (fn [_] nil) (range 10))))
  (is (= [1000000 1000001]
         (->> (mymap inc (range))
              (drop (dec 1000000))
              (take 2)))))

(deftest card-recognizer-test
  (is (= {:suit :diamond :rank 10} (card-recognizer "DQ")))
  (is (= {:suit :heart :rank 3} (card-recognizer "H5")))
  (is (= {:suit :club :rank 12} (card-recognizer "CA")))
  (is (= (range 13) (map (comp :rank card-recognizer str)
                         '[S2 S3 S4 S5 S6 S7
                           S8 S9 ST SJ SQ SK SA]))))

(deftest tit-test
  (is (= (tit '{a {p 1, q 2}
                b {m 3, n 4}})
         '{[a p] 1, [a q] 2
           [b m] 3, [b n] 4}))
  (is (= (tit '{[1] {a b c d}
                [2] {q r s t u v w x}})
         '{[[1] a] b, [[1] c] d,
           [[2] q] r, [[2] s] t,
           [[2] u] v, [[2] w] x}))
  (is (= (tit '{m {1 [a b c] 3 nil}})
         '{[m 1] [a b c], [m 3] nil})))

(deftest trapezoid-test
  (is (= (second (trapezoid [2 3 2])) [2 5 5 2]))
  (is (= (take 5 (trapezoid [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]]))
  (is (= (take 2 (trapezoid [3 1 2])) [[3 1 2] [3 4 3 2]]))
  (is (= (take 100 (trapezoid [2 4 2])) (rest (take 101 (trapezoid [2 2]))))))

(deftest palindromic-numbers-test
  ;; (is (= (take 26 (palindromic-numbers 0))
  ;;        [0 1 2 3 4 5 6 7 8 9
  ;;         11 22 33 44 55 66 77 88 99
  ;;         101 111 121 131 141 151 161]))
  ;; (is (= (take 16 (palindromic-numbers 162))
  ;;        [171 181 191 202
  ;;         212 222 232 242
  ;;         252 262 272 282
  ;;         292 303 313 323]))
  ;; (is (= (take 6 (palindromic-numbers 1234550000))
  ;;        [1234554321 1234664321 1234774321
  ;;         1234884321 1234994321 1235005321]))
  ;; (is (= (first (palindromic-numbers (* 111111111 111111111)))
  ;;        (* 111111111 111111111)))
  ;; (is (= (set (take 199 (palindromic-numbers 0)))
  ;;        (set (map #(first (palindromic-numbers %)) (range 0 10000)))))
  ;; (is (= true
  ;;        (apply < (take 6666 (palindromic-numbers 9999999)))))
  ;; (= (nth (palindromic-numbers 0) 10101)
  ;;    9102019)
  )

(deftest pairwise-disjoint-test
  (is (= (pairwise-disjoint #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
         true))
  (is (= (pairwise-disjoint #{#{:a :b :c :d :e}
                              #{:a :b :c :d}
                              #{:a :b :c}
                              #{:a :b}
                              #{:a}})
         false))
  (is (= (pairwise-disjoint #{#{[1 2 3] [4 5]}
                              #{[1 2] [3 4 5]}
                              #{[1] [2] 3 4 5}
                              #{1 2 [3 4] [5]}})
         true))
  (is (= (pairwise-disjoint #{#{'a 'b}
                              #{'c 'd 'e}
                              #{'f 'g 'h 'i}
                              #{''a ''c ''f}})
         true))
  (is (= (pairwise-disjoint #{#{'(:x :y :z) '(:x :y) '(:z) '()}
                              #{#{:x :y :z} #{:x :y} #{:z} #{}}
                              #{'[:x :y :z] [:x :y] [:z] [] {}}})
         false))
  (is (= (pairwise-disjoint #{#{(= "true") false}
                              #{:yes :no}
                              #{(class 1) 0}
                              #{(symbol "true") 'false}
                              #{(keyword "yes") ::no}
                              #{(class '1) (int \0)}})
         false))
  (is (= (pairwise-disjoint #{#{distinct?}
                              #{#(-> %) #(-> %)}
                              #{#(-> %) #(-> %) #(-> %)}
                              #{#(-> %) #(-> %) #(-> %)}})
         true))
  (is (= (pairwise-disjoint #{#{(#(-> *)) + (quote mapcat) #_ nil}
                              #{'+ '* mapcat (comment mapcat)}
                              #{(do) set contains? nil?}
                              #{, , , #_, , empty?}})
         false)))

(deftest map-defaults-test
  (is (= (map-defaults 0 [:a :b :c]) {:a 0 :b 0 :c 0}))
  (is (= (map-defaults "x" [1 2 3]) {1 "x" 2 "x" 3 "x"}))
  (is (= (map-defaults [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})))

(deftest index-seq-test
  (is (= (index-seq [:a :b :c]) [[:a 0] [:b 1] [:c 2]]))
  (is (= (index-seq [0 1 3]) '((0 0) (1 1) (3 2))))
  (is (= (index-seq [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]])))

(deftest comparisons-test
  (is (= :gt (comparisons < 5 1)))
  (is (= :eq (comparisons (fn [x y] (< (count x) (count y))) "pear" "plum")))
  (is (= :lt (comparisons (fn [x y] (< (mod x 5) (mod y 5))) 21 3)))
  (is (= :gt (comparisons > 0 2))))

(deftest balanced-brackets-test
  (is (balanced-brackets? "This string has no brackets."))
  (is (balanced-brackets? "class Test {
      public static void main(String[] args) {
        System.out.println(\"Hello world.\");
      }
    }"))
  (is (not (balanced-brackets? "(start, end]")))
  (is (not (balanced-brackets? "())")))
  (is (not (balanced-brackets? "[ { ] } ")))
  (is (balanced-brackets? "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))"))
  (is (not (balanced-brackets? "([]([(()){()}(()(()))(([[]]({}([)))())]((((()()))))))")))
  (is (not (balanced-brackets? "["))))
