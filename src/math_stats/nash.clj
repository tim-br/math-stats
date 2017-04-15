(ns math-stats.nash)

(defn foo [bar]
  (cond
    (and (= (first bar) 'a1)
         (= (first (rest bar)) 'b1))
    3

    (and (= (first bar) 'a2)
         (= (first (rest bar)) 'b1))
    -10

    (and (= (first bar) 'a1)
         (= (first (rest bar)) 'b2))
    -1

    (and (= (first bar) 'a2)
         (= (first (rest bar)) 'b2))
    6))

(def ascore (atom 0))

(doseq [e (range 10000)]
  (let [a (rand)
        b (rand)]
    (if (< a 0.2)

      (do
        (if (< b 0.5)
          (swap! ascore #(+ %1 %2) (foo '(a2 b1)))
          (swap! ascore #(+ %1 %2) (foo '(a2 b2)))))
      (do
        (if (< b 0.5)
          (swap! ascore #(+ %1 %2) (foo '(a1 b1)))
          (swap! ascore #(+ %1 %2) (foo '(a1 b2))))))))