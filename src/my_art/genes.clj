(ns my-art.genes)

(defmulti mutate-gene :type)

(defn make-color-gene [& rst]
  {:type :color
   :data rst})

(defn make-random-color-component []
  (rand-int 255))

(defn make-random-color []
  (apply make-color-gene (take 4 (repeatedly make-random-color-component))))

(defmethod mutate-gene :color [gene]
  (let [res (vec (:data gene))
        c (count res)
        i (rand-int c)]
    (assoc gene :data (assoc res i (make-random-color-component)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-triangle-gene [& rst]
  {:type :triangle
   :data rst})

(defn make-random-coordinate []
  (/ (rand-int 1000) 1000))

(defn make-random-triangle []
  (apply make-triangle-gene (take 6 (repeatedly make-random-coordinate))))

(defmethod mutate-gene :triangle [gene]
  (let [res (vec (:data gene))
        c (count res)
        i (rand-int c)]
    (assoc gene :data (assoc res i (make-random-coordinate)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-random-gene []
  (let [possibilities [make-random-color
                       make-random-triangle]]
    ((get possibilities (rand-int (count possibilities))))))

(defn make-random-creature [nr-genes]
  (vec (take nr-genes (repeatedly make-random-gene))))

(defn make-mutated-creature [creature]
  (let [l (count creature)
        i (rand-int l)
        gene (if (> 50 (rand-int 100))
               (mutate-gene (get creature i))
               (make-random-gene))]
    (assoc creature i gene)))
