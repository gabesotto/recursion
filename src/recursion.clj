(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (or(empty? coll) (not (empty? (rest coll))))
    false
    true
    ))

;; I Should have used a cond function instead of nested ifs... too lazy to fix it,
;; I also should have used singleton? instead of (empty? (rest coll))
(defn my-last [coll]
  (if (empty? coll)
    nil
    (if(empty? (rest coll))
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq)       nil
    (empty? (rest a-seq)) (first a-seq)
    :else                (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq)        nil
   (empty? (rest a-seq)) (first a-seq)
   :else                 (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq)        ()
   (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
   :else                 (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)          false
    (== elem (first a-seq)) true
    :else                   (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)         ()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else                  ()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)          ()
   (pred? (first a-seq))   (my-drop-while pred? (rest a-seq))
   :else                   a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))  true
   (not= (count a-seq) (count b-seq))   false
   (= (first a-seq) (first b-seq))      (seq= (rest a-seq) (rest b-seq))
   :else                                false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (== n 0)  0
   (== n 1)  1
   :else     (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
  '()
   (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (inits (reverse (rest (reverse a-seq)))))))
;;^^^^ Super clever, reverse -> rest -> reverse simply strips the last element from the seq instead of the first

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
     (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freq (if (contains? freqs (first a-seq))
                     (update-in freqs [(first a-seq)] inc)
                     (assoc freqs (first a-seq) 1))]
        (my-frequencies-helper new-freq (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
 (if (empty? a-map)
   ()
   (let [ele (first a-map)]
     (concat (repeat (second ele) (first ele)) (un-frequencies (rest a-map))))))

(defn my-take[n coll]
  (if (or (<= n 0) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (concat (my-drop (dec n) (rest coll)))))

(defn halve [a-seq]
  (let
    [half   (int (/ (count a-seq) 2))
     first  (my-take half a-seq)
     second (my-drop half a-seq)]
    (vector first second)))

(defn seq-merge [a-seq b-seq]
   (cond
     (empty? a-seq) b-seq
     (empty? b-seq) a-seq
     (<= (first a-seq) (first b-seq))  (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
     :else                             (cons (first b-seq) (seq-merge  a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
 (if (<= (count a-seq) 1)
   a-seq
   (seq-merge
    (merge-sort (first (halve a-seq)))
    (merge-sort (second (halve a-seq))))))

;;helper functions

(defn inits-rev [a-seq]
  (reverse (inits a-seq)))

(defn decr [seq]
  (cond
   (empty? (rest seq))  true
   (>= (first seq) (second seq)) (decr (rest seq))
   :else false))

(defn incr [seq]
  (cond
   (empty? (rest seq))  true
   (<= (first seq) (second seq)) (incr (rest seq))
   :else false))

(defn incr-or-decr? [seq]
  (or (incr seq) (decr seq)))


;;Extra Credit
(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [ mono   (my-take-while incr-or-decr? (inits-rev a-seq))
           size   (dec (count mono))
           remain (my-drop size a-seq)]
      (cons (last mono) (split-into-monotonics remain)))))

(defn permutations [rot]
)

(permutations '(1 2 3))

(cons 1 '(1 2 3))


;(map permutations (rotations '(1 2 3)))

(defn powerset [a-set]
  [:-])






