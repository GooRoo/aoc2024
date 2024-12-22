(defn parse-input [input]
  (def grammar
    '{:wall (/ "#" :w)
      :empty (/ "." :e)
      :box (/ "O" :b)
      :robot (/ (* (line :row) (column :col) "@") :r)
      :cell (+ :wall :empty :box :robot)
      :line (* (group (some :cell)) "\n")
      :grid (group (some :line))

      :up (/ "^" :up)
      :down (/ "v" :down)
      :left (/ "<" :left)
      :right (/ ">" :right)
      :direction (+ :up :down :left :right (? "\n"))
      :moves (group (some :direction))

      :main (*
             :grid (-> :row) (-> :col)
             "\n"
             :moves)})
  (let [[grid row col moves] (peg/match grammar input)]
    [[(dec row) (dec col)] grid moves]))

(defn draw [grid]
  (do (print
       (string/join (->> grid
                         (map |(->> $
                                    (map |(case $
                                            :w "#"
                                            :e "."
                                            :b "O"
                                            :bl "["
                                            :br "]"
                                            :r "@"))
                                    (string/join))))
                    "\n"))))

(defn step [[row col] direction]
  (case direction
    :up    [(dec row) col]
    :down  [(inc row) col]
    :left  [row (dec col)]
    :right [row (inc col)]))

(defn try-go [pos direction grid]
  (let [one-step (step pos direction)]
    (case (get-in grid one-step)
      :e one-step
      :b (let [new-pos (try-go one-step direction grid)]
           (if (= one-step new-pos)
             pos
             new-pos))
      pos)))

(defn swap [from to grid]
  (let [from-val (get-in grid from)
        to-val (get-in grid to)]
    (-> grid
        (put-in from to-val)
        (put-in to from-val))))

(defn distance [from to]
  (let [[from-row from-col] from
        [to-row to-col] to]
    (math/abs (cond
                (= from-row to-row) (- from-col to-col)
                (= from-col to-col) (- from-row to-row)
                0))))

(defn go! [from to direction grid]
  (let [d (distance from to)
        one-step (step from direction)]
    (case d
      0 from
      1 (do (swap from to grid) to)
      (do (->> grid
               (swap one-step to)
               (swap from one-step))
          one-step))))

(defn simulate [pos grid moves]
  (let [[m & moves] moves
        next-empty (try-go pos m grid)]
    # (print "\n" "Move: " m "\n")
    (def new-pos (go! pos next-empty m grid))
    # (draw grid)
    (if (not (empty? moves))
      (simulate new-pos grid moves))))

(def solve-first
  (do
    # (def filename "data/task.example")
    (def filename "data/task.data")

    (def input (slurp filename))

    (def [start grid moves] (parse-input input))
    (pp start)
    (var field grid)

    (draw field)
    (simulate start field moves)

    (var total 0)
    (for i 0 (length field)
      (for j 0 (length (field 0))
        (if (= :b (get-in field [i j]))
          (+= total (+ (* i 100) j)))))
    total))
