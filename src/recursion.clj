(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (let [tail (rest coll)]
      (if (empty? tail)
        (first coll)
        (my-last tail)))))

(defn max-element [a-seq]
  (defn get-max [a b] (if (> a b) a b))
  (defn inner-max [the-seq c-max]
    (if (empty? the-seq)
      c-max
      (if (empty? (rest the-seq))
        (get-max (first the-seq) c-max)
        (inner-max (rest the-seq) (get-max (first the-seq) c-max)))))
  (if (empty? a-seq)
    nil
    (inner-max (rest a-seq) (first a-seq))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()))

(defn my-drop-while [pred? a-seq]
  (cond 
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (not (= (count a-seq) (count b-seq))) false
    (and (empty? a-seq) (empty? b-seq)) true
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    (empty? seq-2) seq-2
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= 0 k) 
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (= 0 n) 0
    (= 1 n) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1) 
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))


(defn my-range [up-to]
  (if (= 0 up-to) 
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq) '([])
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (defn first-n [n seq-1]
    (cond
      (empty? seq-1) '()
      (< n 1) '()
      :else (cons (first seq-1) (first-n (- n 1) (rest seq-1)))))
  (map (fn [i] (first-n i a-seq)) (range 0 (+ 1 (count a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq) '(())
    (let [inits-1 (inits a-seq)
          tails-1 (tails a-seq)]
      (rest (map concat tails-1 inits-1)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) 
    freqs
    (let [keyvalue (first a-seq)]
      (if (contains? freqs keyvalue) 
        (my-frequencies-helper (assoc freqs keyvalue (+ (get freqs keyvalue) 1)) (rest a-seq))
        (my-frequencies-helper (assoc freqs keyvalue 1) (rest a-seq))))))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [entryk] (repeat (get a-map entryk) entryk)) (keys a-map))))

(defn my-take [n coll]
  (cond
   (empty? coll) coll
   (< n 1) '()
   :else (cons (first coll) (my-take (- n 1) (rest coll)))))


(defn my-drop [n coll]
  (cond
    (empty? coll) coll
    (< n 1) coll
    :else (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [mid (int (/ (count a-seq) 2))]
    (vector (my-take mid a-seq) (my-drop mid a-seq))))

(defn seq-merge [a-seq b-seq]  
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (let [fa (first a-seq)
                fb (first b-seq)]
            (if (<= fa fb)
              (cons fa (seq-merge (rest a-seq) b-seq))
              (cons fb (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (>= 1 (count a-seq))
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))


(defn split-into-monotonics [a-seq]
  (defn is-monot [s-seq]
    (cond
      (empty? s-seq) false
      (apply < s-seq) s-seq
      (apply > s-seq) s-seq
      :else false))

  (cond
    (empty? a-seq)  nil
    (= 1 (count a-seq)) (cons a-seq nil)
    :else (let [mono (some is-monot (reverse (inits a-seq)))]
            (cons mono (split-into-monotonics (drop (count mono) a-seq))))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

