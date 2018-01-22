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
      ;:else (recur (+ i 1) j result (conj col (< (rand 2) 1)))
      :else (recur (+ i 1) j result (conj col true))
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
    (as-> str_ s 
      (count s)
      (- 9 s)
      (repeat s "0")
      (apply str s)
      (str s str_)
      (char-array s)
      (vec s)
      (reverse s)
      (mapv #(if (= \1 %) 1 0) s))))

(defn times_after [cells rule n]
    (->> rule
         (next_permutation result)
         (recur (+ i 1))
         (if (>= i n) result)
         (loop [i 0 result cells])))

(defn print_cells [cells]
  (loop [i 0]
    (when (< i (count cells))
      (println (map #(if % 1 0) (get cells i)))
      (recur (+ i 1)))
      )
  (println))

(defn equal_cells [a b]
  (let [x (-> a (get 0) count) y (count a)]
    (loop [i 0
          j 0]
          (cond
            (= j y) true
            (= i x) (recur 0 (+ j 1))
            (= (get_cell a i j) (get_cell b i j)) (recur (+ i 1) j)
            :else false))))

(defn is_stable [cells rule]
  (equal_cells cells (next_permutation cells rule)))

(defn count_loop [cells rule]
  (loop [states [cells] now (next_permutation cells rule)]
    (if (some #(equal_cells % now) states) (count states) (recur (conj states now) (next_permutation now rule)))))
;warning this function returns (count states)

(defn confirm-all []
  (loop [num_ 0 
         rule (num_to_rule num_) 
         i 0
         sum 0]
    ;(-> (create-cells 10 10) (times_after (num_to_rule num_) 1) print_cells)
    (cond 
     (> num_ 256) nil
     (> i 1000) (do (println rule " " sum) (recur (+ num_ 1) (num_to_rule num_) 0 0))
     :else (recur num_ rule (+ 1 i) (if (is_stable (times_after (create-cells 10 10) rule 1) rule) sum (+ sum 1))))))

;(confirm-all)

;(let [cells (create-cells 10 10)
;      rule (num_to_rule 12)]
;  (print_cells cells)
;  (println)
;  (print_cells (times_after cells rule 100)))
