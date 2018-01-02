(defn get_cell [cells x y]
  (-> cells (get x) (get y)))

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

(defn check_around [cells x y]
    (loop [i -1
           j -1
           result 0]
      (println [i, j] result)
      (cond (> j 1)               result
            (> i 1)               (recur -1 (+ j 1) result)
            (and (= i 0) (= j 0)) (recur (+ i 1) j result)
            :else                 (recur (+ i 1) j (if (get_cell cells (+ i x) (+ j y)) (+ result 1) result))
            )))

(defn next_permutation [cells rule]
  (loop [i 0
         j 0] cells))

(defn print_cells [cells]
  (loop [i 0]
    (when (< i (count cells))
      (println (map #(if % 1 0) (get cells i)))
      (recur (+ i 1)))
      ))


(let [cells (create-cells 10 10)
      rule [1 1 1 1 1 1 1 1]]
  (print_cells cells)
  (println)
  (println (check_around (next_permutation cells rule) 1 1)))
