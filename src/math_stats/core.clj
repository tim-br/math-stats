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
  [probability subset-of-cases total-cases]
  (* (Math/pow probability
               subset-of-cases)
     (Math/pow (- 1 probability)
               (- total-cases subset-of-cases))))

(defn binomial-distribution
  [prob subset-cases total-cases]
  (* (bin-coefficient total-cases subset-cases)
     (prob-of-cases prob subset-cases total-cases)))
