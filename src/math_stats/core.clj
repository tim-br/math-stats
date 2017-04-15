(ns math-stats.core)

(defn fact [x]
  (loop [n x f 1]
    (if (= n 1)
      f
      (recur (dec n) (* f n)))))

(defn bin-coefficient [n k]
  (if (or (= n k) (= k 0))
    1
    (* (/ n k)
       (bin-coefficient (- n 1) (- k 1)))))

(defn prob-of-cases
  "helper function for binomial distribution
  -- needs better name"
  [probability subset-of-cases total-cases]
  (* (Math/pow probability
               subset-of-cases)
     (Math/pow (- 1 probability)
               (- total-cases subset-of-cases))))

(defn binomial-distribution
  [prob subset-cases total-cases]
  (* (bin-coefficient total-cases subset-cases)
     (prob-of-cases prob subset-cases total-cases)))

(defn binom-wrapper
  [prob total-cases small-subset large-subset]
  (let [ret (atom 0)]
    (doseq [i (range small-subset (inc large-subset))]
      (let [res (binomial-distribution prob i total-cases)]
        (swap! ret #(+ % res))))
    @ret))

(defn z-score
  [{:keys [avg standard-deviation x-value]}]
  (/ (- x-value avg)
     standard-deviation))

(defn z-score-to-x-value
  [{:keys [z-score standard-deviation avg]}]
  (+ (* z-score standard-deviation)
     avg))

(defn standard-deviation
  [{:keys [z-score avg x-value]}]
  (/ (- x-value avg)
     z-score))

(defn iterate-list
  [list]
  (let [foo (map #(str %) list)]
    (doseq [b foo]
      (println b))))
(defn gen-chain-rule
  [list]
  (if (= 1 (count list))
    list
    (let [c (first list)
          li (cons '| (rest list))]
      (let [foo (cons c li)]
        (cons foo (gen-chain-rule (rest list)))))))

(defmacro defprob
  [var prob value]
  `(def ~var {:prob ~prob :value ~value}))

(def car {:value 0.3 :prob 'b})

