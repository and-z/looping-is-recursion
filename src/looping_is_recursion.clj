(ns looping-is-recursion)

;; (defn power [base exp]
;;   (cond
;;     (zero? base) 0
;;     (zero? exp) 1
;;     :else (* base (power base (dec exp)))))

;; eval of non-optimized recursive function call
;; (power 2 3)
;; (* 2 (power 2 2))
;; (* 2 (* 2 (power 2 1)))
;; (* 2 (* 2 (* 2 (power 2 0))))
;; (* 2 (* 2 (* 2 1)))
;; (* 2 (* 2 2))
;; (* 2 4)
;; => 8

(defn power [base exp]
  (let [h (fn [product base exp]
            (cond
              (zero? base) 0
              (zero? exp)  product
              :else        (recur (* product base) base (dec exp))))]
    (h 1 base exp)))

;; eval of tail-call optimized recursive function call
;; (power 2 3)
;; (h 1 2 3)
;; (h (* 1 2) 2 2) ;; (h 2 2 2)
;; (h (* 2 2) 2 1) ;; (h 4 2 1)
;; (h (* 4 2) 2 0) ;; (h 8 2 0)
;; => 8

(defn last-element [a-seq]
  (let [h (fn [head tail]
            (if (empty? tail)
              head
              (recur (first tail) (rest tail))))]
    (h (first a-seq) (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [h1 r1 h2 r2]
                 (cond
                   (not= h1 h2) false
                   (and (empty? r1)
                        (empty? r2)) true
                   (not= (count r1) (count r2)) false
                   :else (recur (first r1) (rest r1) (first r2) (rest r2))))]
    (helper (first seq1) (rest seq1) (first seq2) (rest seq2))))

(defn find-first-index [pred a-seq]
  (when (seq a-seq)
    (loop [idx 0
          found (pred (first a-seq))
          tail (rest a-seq)]
     (cond
       found idx
       (empty? tail) nil
       :else (recur (inc idx) (pred (first tail)) (rest tail))))))

(defn avg [a-seq]
  (loop [sum (first a-seq)
         divisor 1
         tail (rest a-seq)]
    (if (empty? tail)
      (/ sum divisor)
      (recur (+ sum (first tail)) (inc divisor) (rest tail)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set el]
                 (if (contains? a-set el)
                   (disj a-set el)
                   (conj a-set el)))]
    (loop [odds #{}
           elements a-seq]
     (if (empty? elements)
       odds
       (recur (toggle odds (first elements)) (rest elements))))))

(defn fast-fibo [n]
  (loop [a 0
         b 1
         c 0]
    (cond
      (= n c) a
      :else (recur b (+ a b) (inc c)))))

(defn cut-at-repetition [a-seq]
  (loop [result []
         els #{}
         tail a-seq]
    (let [el (first tail)]
      (if (or (contains? els el)
              (empty? tail))
        result
        (recur (conj result el) (conj els el) (rest tail))))))

