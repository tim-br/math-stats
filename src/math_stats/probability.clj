(ns math-stats.probability
  (:require [clojure.math.combinatorics :as combo]))

(def state (atom {}))

(defn add-probability
  [name value]
  (swap! state assoc name value))

(defn not-prop
  [prop]
  (- 1 (get @state prop)))

(defn get-prop
  [prop]
  (get @state prop))

(defn gen-chain-rule
  [list]
  (if (= 1 (count list))
    list
    (let [c (first list)
          li (cons '| (rest list))]
      (let [foo (cons c li)]
        (cons foo (gen-chain-rule (rest list)))))))

(def li '( '( w |  s r) '(s | r) '(r | c)  'c))
(def li2 '( ( w |  s r) (s | c) (r | c)  c))
(def li3 '( ( w |  (not s) r) (s | c) (r | c)  c))

(def value-map {'(w | s r)                   0.99
                '((not w) | s r)             0.01
                '(w | (not r) s)             0.9
                '((not w) | (not r) s)       0.1
                '(w | (not r) (not s))       0
                '((not w) | (not s) (not r)) 1
                '(w | r (not s))             0.9
                '((not w) | r (not s))       0.1
                'c                           0.5
                '(not c)                     0.5
                '(s | c)                     0.1
                '((not s) | c)               0.9
                '(s | (not c))               0.5
                '((not s) | (not c))         0.5
                '(r | c)                     0.8
                '((not r) | c)               0.2
                '(r | (not c))               0.2
                '((not r) | (not c))         0.8})

(defn get-priors
  [li]
  (if (= 0 (count li))
    '()
    (let [elem (first (rest li))]
      (if (= elem '|)
        (rest (rest li))
        (get-priors (rest li))))))

(defn get-main
  [li list-helper]
  (if (= 0 (count li))
    list-helper
    (let [second-elem (first (rest li))]
      (if (= second-elem '|)
        (do
          (reverse (cons (first li) list-helper)))
        (get-main  (rest li) (cons (first li) list-helper))))))

(defn get-main-wrapper
  [li]
  (get-main li '()))

(defn prob-for-elem
  [elem]
  (get value-map elem))

(defn combine-probs
  "make a conditional probability based on priors and the givens"
  [given priors]
  (let [foo (concat given '(|))]
    (concat foo priors)))

(defn permutate-priors
  [prob]
  (let [priors (get-priors prob)
        given (get-main-wrapper prob)
        perms (combo/permutations priors)]
    (map #(combine-probs given %) perms)))

(defn permutate-given
  [cond-prob]
  (let [given (get-main-wrapper cond-prob)
        priors (get-priors cond-prob)
        perms (combo/permutations given)]
    (map #(combine-probs % priors) perms)))

(defn probability
  [cond-prob]
  (if (not (list? cond-prob))
    (get value-map cond-prob)
    (if (= 'not (first cond-prob))
      (get value-map cond-prob)
      (let [given-permutation (permutate-given cond-prob)
            priors-permutation (permutate-priors cond-prob)]
        (loop [e priors-permutation]
          (if (empty? e)
            (println "done")
            (if (not (nil? (get value-map (first e))))
              (get value-map (first e))
              (recur (rest e)))))))))

(defn not-probability
  [cond-prob]
  (let [prob (probability cond-prob)
        times-100 (* 100 prob)]
    (/ (- 100 times-100)
       100)))

(defn process-list
  [li]
  (reduce #(* (bigdec %1) (bigdec %2)) (map #(probability %) li)))

(defn swap-with-not
  [elem list]
  (if (not (seq? list))
    (do
      (if (= list elem)
        `(~'not ~elem)
        list))
    (if (= 0 (count list))
      list
      (let [el (first list)]
        (if (= elem el)
          (let [new-elem `(~'not ~elem)]
            (do
              (cons new-elem (swap-with-not elem (rest list)))))
          (cons el (swap-with-not elem (rest list))))))))

(defn swap-with-not-sublists
  [lists elem]
  (doall (map #(swap-with-not elem %) lists)))

(defn swap-multiple-elems
  [elems list]
  (reduce swap-with-not-sublists list elems))

(doseq [e (range (count '(w s c r)))]
  (doseq [e2 (combo/combinations '(w s c r) (inc e))]
    (println (process-list (swap-multiple-elems e2 li2)))))

(defn process-multiple-lists
  [elems li]
  (doseq [e (range (count elems))]
    (doseq [e2 (combo/combinations elems (inc e))]
      (println (swap-multiple-elems e2 li)))))

(comment
  (process-list li3)
  (not-probability '(s | (not c)))
  (not-probability '(w | (not r) s))
  (not-probability '(w | (not r) (not s)))

  (swap-with-not-sublists '((w | (not s) r) ((not s) | c) (r | c) c) 'r))


(defn replace-word
  [list word]
  (if (= 0 (count list))
    list
    (if (= (first list) word)
      (cons "FOO" (replace-word (rest list) word))
      (cons (first list) (replace-word (rest list) word)))))

(defn bigdec-add
  [li]
  (reduce #(+ (bigdec %1) (bigdec %2)) 0 li))