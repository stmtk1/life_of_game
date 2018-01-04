(defn get_cell [cells x y]
  (-> cells (get y) (get x)))

(defn create-cells [width height]
  (loop [i 0
         j 0
         result []
         col []]
    (cond
      (= j height) result
      (= i width) (recur 0 (+ j 1) (conj result col) [])
      :else (recur (+ i 1) j result (conj col (< (rand 2) 1)))
      )))

(defn check_around [x y cells]
    (loop [i -1
           j -1
           result 0]
      (cond (> j 1)               result
            (> i 1)               (recur -1 (+ j 1) result)
            (and (= i 0) (= j 0)) (recur (+ i 1) j result)
            :else                 (recur (+ i 1) j (if (get_cell cells (+ i x) (+ j y)) (+ result 1) result))
            )))

(defn next_permutation [cells rule]
  (let [end_x (-> cells (get 0) count (- 1))
        end_y (- (count cells) 1)]
    (loop [i 0
           j 0
           result []
           col []] 
      (cond (> j end_y) result
            (> i end_x) (recur 0 (+ j 1) (conj result col) [])
            :else       (recur (+ i 1) j result (->> cells (check_around i j) (get rule) (= 1) (conj col)))
            ))))

(defn num_to_rule [num_]
  (let [str_ (Integer/toString num_ 2)]
    (mapv #(if (= \1 %) 1 0)
      (-> (apply str (repeat (- 9 (count str_)) "0"))
        (str str_)
        char-array
        vec
        reverse
        ))))

(defn times_after [cells rule n]
  (loop [i 0
         result cells
         ]
    (if (>= i n) 
      result
      (recur (+ i 1) (next_permutation result rule)))))

(defn print_cells [cells]
  (loop [i 0]
    (when (< i (count cells))
      (println (map #(if % 1 0) (get cells i)))
      (recur (+ i 1)))
      ))


(println (num_to_rule 12))

(let [cells (create-cells 10 10)
      rule (num_to_rule 12)]
  (print_cells cells)
  (println)
  (print_cells (times_after cells rule 100)))
