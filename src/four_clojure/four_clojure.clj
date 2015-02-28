(ns four-clojure.four_clojure
  (:gen-class))

;; problem 26 - fibbnaci
(fn [n]
  (map first
       (take n
             (iterate #(vector (second %) (+ (first %) (second %))) [1 1]))))

#(take % (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))

;; problem 27 - palindrome detector
#(= (seq %) (reverse %))

;; problem 28 - Flatten a Sequence
;; Write a function which flattens a sequence.
(defn myflat
  [sq]
  (filter #(not (coll? %)) (tree-seq coll? identity sq)))

(fn flat
  ([coll] (flat [] coll))
  ([res coll]
     (map #(if (seq? %)
             (flat res %)
             (conj res %)) coll)))

(fn flat
  ([coll] (flat coll []))
  ([coll res]
     (letfn
         [(add-items [subcoll]
            (for [x subcoll]
              (if (coll? x)
                (add-items x)
                (conj res x))))]
       (add-items coll))))

(fn flat
  ([coll] (flat [] coll))
  ([res coll]
     (letfn [(add-items [subcoll]
               (if (coll? subcoll)
                 (mapcat add-items subcoll)
                 subcoll))]
       (into [] (add-items coll)))))

;; problem 34 - implement range
#(take (- %2 %1) (iterate inc %1))

(defn my-range
  "Problem 34 - implement range."
  ([end] (my-range 0 end))
  ([start end] (my-range start end 1))
  ([start end step]
     (for [x (take (- end start) (iterate #(+ % step) start)) :when (< x end)] x)))

;; problem 41 - drop every nth item
(defn drop-every-nth
  [col n]
  (mapcat #(if (< (count %) n) % (butlast %)) (partition-all n col)))

#(apply concat (partition-all (- %2 1) %2 %)) ; partition-all has this functionality

;; problem 43 - Reverse Interleave
;; Write a function which reverses the interleave process into x number of subsequences.
(defn reverse-interleave
  [sq x]
  (vals (group-by #(rem % x) sq)))

;; problem 44 - Rotate Sequence
;; Write a function which can rotate a sequence in either direction.
(defn rotate-sequence
  [n aseq]
  (let [x (Math/abs (rem n (count aseq)))]
    (if (pos? n)
     (concat (drop x aseq) (take x aseq))
     (concat (take-last x aseq) (drop-last x aseq)))))

(defn rotate-sequence2
  [x sq]
  (let [n (count sq)]
    (take n (drop (mod x n) (cycle sq)))))

;; problem 46 - Flipping out
;; Write a higher-order function which flips the order of the arguments of an
;; input function.
(defn flip-out [f] #(f %2 %1))

;; problem 50 - Split by Type
;; Write a function which takes a sequence consisting of items with different
;; types and splits them up into a set of homogeneous sub-sequences. The
;; internal order of each sub-sequence should be maintained, but the
;; sub-sequences themselves can be returned in any order (this is why 'set' is
;; used in the test cases).
(defn split-by-type
  [sq]
  (vals (group-by class sq)))

;; problem 54 - Partition a Sequence
;; Write a function which returns a sequence of lists of x items each. Lists of
;; less than x items should not be returned.
(defn mypartition [n sq]
  (let [[a b] (split-at n sq)]
    (cond
      (seq b) (conj (mypartition n b) a)
      (= (count a) n) (conj '() a)
      :t '())))

(defn mypartition-1 [n sq]
  (letfn [(myp [n sq]
            (let [[a b] (split-at n sq)]
              (lazy-seq
               (conj a (myp n b)))))]
    (take 1 (myp n sq))))

(defn mypartition-2
  ([n sq] (mypartition-2 n 0 sq))
  ([n d sq]
     (let [s (drop d sq) v (take n s)]
       (if (and (seq s) (= n (count v)))
         (conj (mypartition-2 n (+ d n) sq) (take n s))
         '()))))

;; problem 55 - Count Occurrences
;; Write a function which returns a map containing the number of occurences of
;; each distinct item in a sequence.
(defn count-occurs
  [sq]
  (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} sq))

;; problem 56 - Find Distinct Items
;; Write a function which removes the duplicates from a sequence. Order of the
;; items must be maintained.
(defn distinct-items
  [sq]
  (reduce #(if (some #{%2} %) % (conj % %2)) [] sq))

;; problem 58 - Function Composition
;; Write a function which allows you to create function compositions. The
;; parameter list should take a variable number of functions, and create a
;; function applies them from right-to-left.
(defn fun-comp
  [& fs]
  (fn [& args]
    (let [rfs (reverse fs)]
      (reduce (fn [a f] (f a)) (apply (first rfs) args) (rest rfs)))))

;; ummels's solution:
(fn mycomp
  ([f] f)
  ([f & fs]
     (fn [& args]
       (f (apply (apply mycomp fs) args)))))

;; chouser's solution:
(fn c [a & r]
  (if r #(a (apply (apply c r) %&))
      a))

;; problem 59 - Juxtaposition
;; Take a set of functions and return a new function that takes a variable
;; number of arguments and returns a sequence containing the result of applying
;; each function left-to-right to the argument list.
(defn juxtaposition
  [& funs]
  (fn [& args] (map #(apply % args) funs)))

;; problem 62 - Re-implement Iterate
;; Given a side-effect free function f and an initial value x write a function
;; which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))),
;; etc.
(defn myiterate
  [f x]
  (lazy-seq
     (cons x (myiterate f (f x)))))

;; problem 63 - group a sequence
(fn [f & args]
  (let [maps (apply map #(hash-map (f %) [%]) args)]
    (apply merge-with concat maps)))

;; problem 65 - Black Box Testing
;; Clojure has many sequence types, which act in subtly different ways. The core
;; functions typically convert them into a uniform "sequence" type and work with
;; them that way, but it can be important to understand the behavioral and
;; performance differences so that you know which kind is appropriate for your
;; application.  Write a function which takes a collection and returns one of
;; :map, :set, :list, or :vector - describing the type of collection it was
;; given.  You won't be allowed to inspect their class or use the built-in
;; predicates like list? - the point is to poke at them and understand their
;; behavior.
;; Special Restrictions: class, type, Class, vector?, sequential?, list?,
;;  seq?, map?, set?, instance?, getClass
(defn problem-65
  [s]
  (if (= 0 (count (flatten (into s {:s 1})))) :map
      (let [n (count s)
            t (conj s :p :p :s :s)]
        (cond (= (count t) (+ n 2)) :set
              (= (first t) :s) :list
              true :vector))))

;; problem 66 - Greatest Common Divisor of two numbers
#(if (zero? %2) %1
     (recur %2 (rem %1 %2)))

;; TODO problem 67 - Prime Numbers
;; Write a function which returns the first x number of prime numbers.
(defn prime-numbers
  "Returns lazy seq of primes."
  [n]
  (loop [p [] i 2 n n]
    (if (> n 0)
      (if (some #(= 0 %) (map #(rem i %) p))
        (recur p (inc i) n)
        (recur (conj p i) (inc i) (dec n)))
      p)))

;; problem 70 - Word Sorting
;; Write a function that splits a sentence up into a sorted list of
;; words. Capitalization should not affect sort order and punctuation should be
;; ignored.
(defn word-sorting
  [s]
  (sort-by #(.toLowerCase %) (.split s "\\W")))

;; problem 74 - Filter Perfect Squares
;; Given a string of comma separated integers, write a function which returns
;; a new comma separated string that only contains the numbers which are perfect
;; squares.
(defn filter-perfect-squares
  [s]
  (clojure.string/join
   "," 
   (filter #(let [i (Integer/parseInt %)
                  x (Math/sqrt i)]
              (and (< 3 i)
                   (= 0.0 (- x (Math/floor x)))))
           (.split s ","))))

;; problem 76 - Intro to Trampoline
;; The trampoline function takes a function f and a variable number of
;; parameters. Trampoline calls f with any parameters that were supplied. If f
;; returns a function, trampoline calls that function with no arguments. This is
;; repeated, until the return value is not a function, and then trampoline returns
;; that non-function value. This is useful for implementing mutually recursive
;; algorithms in a way that won't consume the stack.
(defn my-trampoline
  ([f & a] (my-trampoline (apply f a)))
  ([f]
     (if (fn? f)
       (recur (f))
       f)))

;; problem 78 - Reimplement Trampoline
;; Reimplement the function described in "Intro to Trampoline".
;; (see in tests)

;; cgrant's solution:
#(if (fn? %) (recur (apply % %&) ()) %)

;; problem 80 - Perfect Numbers
;; A number is "perfect" if the sum of its divisors equal the number itself.
;; 6 is a perfect number because 1+2+3=6. Write a function which returns true
;; for perfect numbers and false otherwise.
(defn perfect-number?
  [x]
  false)

;; problem 85 - Happy numbers
;; Happy numbers are positive integers that follow a particular formula:
;; take each individual digit, square it, and then sum the squares to get
;; a new number. Repeat with the new number and eventually, you might get
;; to a number whose squared sum is 1. This is a happy number. An unhappy
;; number (or sad number) is one that loops endlessly. Write a function
;; that determines if a number is happy or not.


;; problem 88 - implement set difference
(fn [a b] (set (into (filter #(not (a %)) b) (filter #(not (b %)) a))))

;; problem 95 - write a predicate which determines whether given sequence is a binary tree
(defn bintree?
  "Determines whether given sequence is a binary tree."
  [aseq]
  (cond
   (nil? aseq) true
   (and (coll? aseq) (= (count aseq) 3)) (every? bintree? (rest aseq))
   :else false))

;; problem 96 - Beauty is symetry
;; Let us define a binary tree as "symmetric" if the left half of the tree is
;; the mirror image of the right half of the tree. Write a predicate to
;; determine whether or not a given binary tree is symmetric. (see To Tree, or
;; not to Tree for a reminder on the tree representation we're using).
(defn symetric-bintree?
  [[n l r]]
  (= (flatten l)
     (filter #(not (coll? %)) (tree-seq coll? (fn [[_ a b]] [_ b a]) r))))

;; maximental's: #(= % ((fn m [[v l r]] (if v [v (m r) (m l)])) %))

;; problem 97 - return nth row of Pascal's triangle
(defn pascal-nth
  "Returns nth row of Pascal's triangle."
  [n]
  (loop [row [1] n n]
    (if (= n 1) row
        (recur (flatten [1 (map #(apply + %) (partition 2 1 row)) 1]) (dec n)))))

;; problem 99
(fn [x y] (map #(- (int %) 48) (str (* x y))))

;; problem 100 - Least Common Multiple
(fn [& args]
  (letfn [(gcd [a b]
            (if (zero? b) a
                (recur b (rem a b))))
          (lcm [a b]
            (/ (* a b) (gcd a b)))]
    (reduce lcm args)))

;; problem 102 - intoCamelCase 
;; When working with java, you often need to create an object with
;; fieldsLikeThis, but you'd rather work with a hashmap that has
;; :keys-like-this until it's time to convert. Write a function which takes
;; lower-case hyphen-separated strings and converts them to camel-case strings.
(defn into-camel-case
  [s]
  (let [w (re-seq #"[^-]+" s)]
    (apply str (first w)
           (map #(apply str (.toUpperCase (str (first %))) (rest %))
                (rest w)))))

(defn into-camel-case2
  [[c :as s]]
  (apply str c
         (map
          (fn [[a b]]
            (cond (= \- a) (Character/toUpperCase b)
                  (= \- b) nil
                  :e b))
          (partition-all 2 1 s))))

;; problem 107
(fn [n] #(Math/pow % n))
(fn [n] #(apply * (repeat n %)))

;; problem 110 - Sequence of pronunciations
;; Write a function that returns a lazy sequence of "pronunciations" of a
;; sequence of numbers. A pronunciation of each element in the sequence consists
;; of the number of repeating identical numbers and the number itself. For
;; example, [1 1] is pronounced as [2 1] ("two ones"), which in turn is
;; pronounced as [1 2 1 1] ("one two, one one").
;; Your function should accept an initial sequence of numbers, and return an
;; infinite lazy sequence of pronunciations, each element being a pronunciation
;; of the previous element.

(defn pronunciation
  "Returns pronunciations."
  [v]
  ;;TODO how to make this a lazy seq?
  (lazy-seq
   (reduce #(conj % (val %2) (key %2)) [] (frequencies v))))

;; problem 118 - Re-implement Map
;; Map is one of the core elements of a functional programming language. Given
;; a function f and an input sequence s, return a lazy sequence of (f x) for
;; each element x in s.
(defn mymap
  [f sq]
  (lazy-seq
   (if (seq sq)
     (cons (f (first sq)) (mymap f (next sq)))
     '())))

;; problem 120
(fn [nums]
  (letfn
    [(num-to-seq [x] (map #(- (int %) 48) (str x)))
     (sum-squares [col] (reduce #(+ (* %2 %2) %1) 0 col))]
    (count (filter #(< % (sum-squares (num-to-seq %))) nums))))

(comp count filter) (fn [n] (< n (apply + (map #(* % %) (map #(- (int %) 48) (str n))))))

;; problem 122 - read a binary number
#(Integer/parseInt % 2)

;; problem 128 - Recognize Playing Cards
;; A standard American deck of playing cards has four suits - spades, hearts,
;; diamonds, and clubs - and thirteen cards in each suit. Two is the lowest
;; rank, followed by other integers up to ten; then the jack, queen, king,
;; and ace.
;; It's convenient for humans to represent these cards as suit/rank pairs, such
;; as H5 or DQ: the heart five and diamond queen respectively. But these forms
;; are not convenient for programmers, so to write a card game you need some way
;; to parse an input string into meaningful components. For purposes of
;; determining rank, we will define the cards to be valued from 0 (the two) to
;; 12 (the ace).
;; Write a function which converts (for example) the string "SJ" into a map of
;; {:suit :spade, :rank 9}. A ten will always be represented with the single
;; character "T", rather than the two characters "10".
(defn card-recognizer
  [[suit rank]]
  (let [suits {\C :club, \D :diamond, \H :heart, \S :spade}
        ranks {\2 0, \3 1, \4 2, \5 3, \6 4, \7 5, \8 6, \9 7,
               \T 8, \J 9, \Q 10, \K 11, \A 12}]
    {:suit (suits suit) :rank (ranks rank)}))

;; #(hash-map :suit ({\C :club, \D :diamond, \H :heart, \S :spade} (first %))
;;           :rank ({\2 0, \3 1, \4 2, \5 3, \6 4, \7 5, \8 6, \9 7, \T 8, \J 9, \Q 10, \K 11, \A 12} (second %)))

;; problem 135 - infix calculator
(fn [& expr]
  (letfn
      [(prefix [[x op & args]]
         (if (nil? op) x
             (op (prefix args) x)))]
    (prefix (reverse expr))))

(fn i ([r] r) ([l o r & m] (apply i (o l r) m)))
(fn calc
  ([n] n)
  ([n1 op n2 & es] (apply calc (op n1 n2) es)))

;; problem 143 - calculate dot product
#(apply + (map * %1 %2))

;; problem 146 - Trees into tables
(defn tit
  [m]
  (into {} (for [k (keys m) [sk v] (m k)] {[k sk] v})))

;; #(into {} (for [[k v] % [sk sv] v] {[k sk] sv}))

;; problem 147 - Pascal's trapezoid
(defn trapezoid
  [v] (iterate #(vec (map +' (cons 0 %) (conj % 0))) v))

;; problem 150 - Palindromic Numbers
;; A palindromic number is a number that is the same when written forwards or
;; backwards (e.g., 3, 99, 14341).
;; Write a function which takes an integer n, as its only argument, and returns
;; an increasing lazy sequence of all palindromic numbers that are not less than n.
;; The most simple solution will exceed the time limit!
(defn palindromic-numbers
  [n]
  ;; (lazy-seq )
  (let [s (str "TODO")] (= s (apply str (reverse s)))))

;; problem 153 - Pairwise Disjoint Sets
;; Given a set of sets, create a function which returns true if no two of those
;; sets have any elements in common1 and false otherwise. Some of the test
;; cases are a bit tricky, so pay a little more attention to them.
;;
;; Such sets are usually called pairwise disjoint or mutually disjoint.

(defn pairwise-disjoint
  [sets] (let [all (apply concat sets)]
           (= (count all) (count (into #{} all)))))

;; problem 156 - Map Defaults
;; When retrieving values from a map, you can specify default values in case the
;; key is not found:
;; (= 2 (:foo {:bar 0, :baz 1} 2))
;; However, what if you want the map itself to contain the default values? Write
;; a function which takes a default value and a sequence of keys and constructs
;; a map.
(defn map-defaults
  [v ks]
  (zipmap ks (repeat v)))

;; problem 157 - Indexing Sequences
;; Transform a sequence into a sequence of pairs containing the original
;; elements along with their index.
(defn index-seq [v] (map-indexed #(vector %2 %) v))

;; problem 166 - Comparisons
;; For any orderable data type it's possible to derive all of the basic
;; comparison operations (<, ≤, =, ≠, ≥, and >) from a single operation (any
;; operator but = or ≠ will work). Write a function that takes three arguments,
;; a less than operator for the data and two items to compare. The function
;; should return a keyword describing the relationship between the two
;; items. The keywords for the relationship between x and y are as follows:
;;    x = y → :eq
;;    x > y → :gt
;;    x < y → :lt
(defn comparisons
  [f x y]
  (let [a (f x y) b (f y x)]
    (cond
     (and a (not b)) :lt
     (and b (not a)) :gt
     :t :eq)))
