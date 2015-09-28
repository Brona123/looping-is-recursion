(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (= 1 n)
                   acc
                   (recur (* acc base) (dec n))))]
    (if (zero? exp)
      1
      (helper base exp))))

(defn last-element [a-seq]
  (if (<= (count a-seq) 1)
    (first a-seq)
    (last-element (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [h-seq1 h-seq2 flag]
                 (if (= (first h-seq1) (first h-seq2))
                   (if (and (> (count h-seq1) 0) (> (count h-seq2) 0))
                     (recur (rest h-seq1) (rest h-seq2) true)
                      true)
                   false))]
    (if (= (count seq1) (count seq2))
      (helper seq1 seq2 true)
      false)))

(defn find-first-index [pred a-seq]
  (loop [index 0]
    (if (>= index (count a-seq))
      nil
      (if (pred (get a-seq index))
        index
        (recur (inc index))))))

(defn avg [a-seq]
  (loop [sum 0
         index 0]
    (if (>= index (count a-seq))
      (/ sum (count a-seq))
      (recur (+ sum (get a-seq index)) (inc index)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [odd-set #{}
         index 0]
    (if (= index (count a-seq))
      odd-set
      (recur (toggle odd-set (get a-seq index)) (inc index)))))

(defn fast-fibo [n]
  (loop [previous 0
         current 0
         index 0]
    (cond
      (= index n) (+ previous current)
      (<= index 1) (recur 0 1 (inc index))
      :else (recur current (+ previous current) (inc index)))))

(defn cut-at-repetition [a-seq]
  (loop [my-seq (subvec a-seq 0 1)
         current-elem nil
         index 0]
    (cond
     (> index (count a-seq)) my-seq
     (zero? index) (recur my-seq (get a-seq 1) 2)
     (= (first a-seq) current-elem) my-seq
     :else (recur (conj my-seq current-elem) (get a-seq index) (inc index)))))
