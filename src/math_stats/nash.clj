(ns math-stats.nash
  (:require [clojure.data.generators :as gen]))

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

(def a-nash 0.8)
(def b-nash 0.35)
(def a-stat (rand))
(def b-stat (rand))

(doseq [x (range 101)]
  (let [b-nash (#(* (bigdec %) (bigdec 0.01)) x)]
     #_[gen/*rnd* (java.util.Random. 9385949482)]
    (do
      (reset! ascore 0)
      (doseq [e (range 1000000)]
        (let [a (gen/double)
              b (gen/double)]
          (if (< a a-nash)

            (do
              (if (< b b-nash)
                (swap! ascore #(+ %1 %2) (foo '(a1 b1)))
                (swap! ascore #(+ %1 %2) (foo '(a1 b2)))))
            (do
              (if (< b b-nash)
                (swap! ascore #(+ %1 %2) (foo '(a2 b1)))
                (swap! ascore #(+ %1 %2) (foo '(a2 b2))))))))
      (println (str "a-nash " a-nash "-- b-nash " b-nash ": " @ascore)))))

(binding [gen/*rnd* (java.util.Random. 928495999222244583)]
  (do
    (reset! ascore 0)
    (doseq [e (range 10000)]
      (let [a (gen/double)
            b (gen/double)]
        (if (< a a-nash)

          (do
            (if (< b b-nash)
              (swap! ascore #(+ %1 %2) (foo '(a1 b1)))
              (swap! ascore #(+ %1 %2) (foo '(a1 b2)))))
          (do
            (if (< b b-nash)
              (swap! ascore #(+ %1 %2) (foo '(a2 b1)))
              (swap! ascore #(+ %1 %2) (foo '(a2 b2))))))))
    (println @ascore)))

(doseq [y (range 11)]
  (doseq [x (range 11)]
    (let [b-nash (#(* (bigdec %) (bigdec 0.1)) x)
          a-nash (#(* (bigdec %) (bigdec 0.1)) y)]
      (binding [gen/*rnd* (java.util.Random. 437)]
        (do
          (reset! ascore 0)
          (doseq [e (range 10000)]
            (let [a (gen/double)
                  b (gen/double)]
              (if (< a a-nash)

                (do
                  (if (< b b-nash)
                    (swap! ascore #(+ %1 %2) (foo '(a1 b1)))
                    (swap! ascore #(+ %1 %2) (foo '(a1 b2)))))
                (do
                  (if (< b b-nash)
                    (swap! ascore #(+ %1 %2) (foo '(a2 b1)))
                    (swap! ascore #(+ %1 %2) (foo '(a2 b2))))))))
          (println (str "a-nash " a-nash "-- b-nash " b-nash ": " @ascore)))))))
