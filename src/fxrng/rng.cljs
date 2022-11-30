(ns fxrng.rng)

(defn fxrand
  ([] (js/fxrand))
  ([maximum] (* maximum (fxrand)))
  ([minimum maximum] (+ minimum (fxrand (- maximum minimum))))
  ([minimum maximum exponent] (+ minimum
                                 (* (- maximum minimum)
                                    (Math/pow (fxrand) exponent)))))

(defn fxrand-geom [p]
  (if (< (fxrand) p)
    1
    (inc (fxrand-geom p))))

(defn fxrand-exp [& [lambda]]
  (/ (- (Math/log (fxrand))) lambda))

(defn fx-flat-dirichlet [n]
  (let [raw-samples (repeatedly n (fxrand-exp))
        sum (apply + raw-samples)]
    (mapv #(/ % sum) raw-samples)))

(defn fxchance [chance]
  (> chance (fxrand)))

(defn fxchoice [option-weight-map]
  (let [weight-sum (apply + (vals option-weight-map))]
    (loop [choice-index (fxrand)
           option-weight-pairs (seq option-weight-map)]
      (if (= 1 (count option-weight-pairs))
        (first (first option-weight-pairs))
        (let [[option weight] (first option-weight-pairs)
              new-index (- choice-index (/ weight weight-sum))]
          (if (<= new-index 0)
            option
            (recur new-index
                   (rest option-weight-pairs))))))))

(defn fxrand-int
  ([maximum] (Math/floor (fxrand maximum)))
  ([minimum maximum] (+ minimum (fxrand-int (- maximum minimum)))))

(defn fxrand-nth [coll]
  (nth coll
       (Math/floor (* (fxrand) (count coll)))))

(defn fxshuffle [coll]
  (loop [sorted-coll coll
         shuffled-list ()]
    (let [size (count sorted-coll)]
      (if (pos? size)
        (let [index (Math/floor (fxrand size))]
          (recur (concat (take index sorted-coll)
                         (drop (inc index) sorted-coll))
                 (conj shuffled-list (nth sorted-coll index))))
        shuffled-list))))

(defn fxrand-paretto [shape scale]
  (/ (Math/pow (* shape scale) shape)
     (Math/pow (fxrand) (+ shape 1))))