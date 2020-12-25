(ns jurassic-jigsaw
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn column [grid col]
  (str/join (map #(nth % col) grid)))

(defn edges [grid]
  {:north (nth grid 0)
   :east (column grid 9)
   :south (nth grid 9)
   :west (column grid 0)})

(defn tiles [file]
  (for [tile (str/split (slurp file) #"\n\n")]
    (let [[name grid-lines] (str/split tile #":\n")
          grid (str/split-lines grid-lines)]
      {:id (read-string (re-find #"\d+" name))
       :grid grid
       :edges (edges grid)})))

(defn count-pixels [edge]
  (count (filter #{\#} edge)))

(defn edge-match [e1 e2]
  (or (= e1 e2)
      (= (str/join (reverse e1)) e2)))

(defn match-any-edges [tile1 tile2]
  (if (= (:id tile1) (:id tile2))
    []
    (let [matches
          (for [edge1 (vals (:edges tile1))
                edge2 (vals (:edges tile2))
                :when (edge-match edge1 edge2)]
            edge1)]
      (if (seq matches)
        [(:id tile1) (:id tile2) (first matches)]
        []))))

(defn match-all-tiles [tiles]
  (remove empty?
          (for [tile1 tiles
                tile2 tiles]
            (match-any-edges tile1 tile2))))

(defn tile-links [tiles]
  (let [matches (match-all-tiles tiles)]
    (->> matches
         (group-by first)
         (map (fn [[k v]] [k (into {} (map #(vec (rest %)) v))]))
         (into {}))))

(defn tiles-with-n-links [tiles n]
  (filter (fn [[k v]] (= n (count v))) (tile-links tiles)))

(defn find-corners [tiles]
  (into {} (tiles-with-n-links tiles 2)))

(defn find-edges [tiles]
  (into {} (tiles-with-n-links tiles 3)))

(defn first-star [tiles]
  (let [corner-ids (keys (find-corners tiles))]
    (apply * corner-ids)))

(defn find-edge-row [edges corners chain connections]
  (let [current (last chain)
        next (first (keys connections))]
    (if-let [next-corner (get corners next)]
      (conj chain next)
      (if-let [next-edge (get edges next)]
        (recur (dissoc edges next)
               corners
               (conj chain next)
               (dissoc next-edge current))))))

(defn filtered-links [edges accepted]
  (->> edges
       (map (fn [[k v]] [k (select-keys v accepted)]))
       (into {})))

(defn remove-matching-link [links row]
  (let [seen (into #{} row)]
    (remove (fn [[from _ _]] (seen from)) links)))

(defn fill-remaining-rows [rows tile-links]
  (if (empty? tile-links)
    rows
    (let [next-row
          (for [elem (last rows)]
            (some (fn [[from to _]] (when (= to elem) from))
                  tile-links))]
      (recur (conj rows (vec next-row))
             (remove-matching-link tile-links next-row)))))

(defn edge-chain [tiles]
  (let [corners (find-corners tiles)
        edges (find-edges tiles)
        edges-and-corners (concat (keys corners) (keys edges))
        [id connections] (first corners)
        first-row (find-edge-row (filtered-links edges edges-and-corners)
                                 (dissoc corners id)
                                 [id]
                                 connections)]
    (fill-remaining-rows
     [first-row]
     (remove-matching-link (match-all-tiles tiles) first-row))))

(defn find-position [grid id]
  (ffirst
   (remove empty?
           (keep-indexed (fn [y row]
                           (keep-indexed (fn [x v] (when (= id v) [x y]))
                                         row))
                         grid))))

(defn neighbors [grid id]
  (let [position (find-position grid id)]
    (into {}
          (for [[direction neighbor]
                {:north [0 -1] :east [1 0]
                 :south [0 1] :west [-1 0]}
                :let [coord (map + neighbor position)
                      value (get-in grid (reverse coord))]
                :when value]
            [direction value]))))

(comment
  (find-position [[1 2 3]
                  [4 5 6]
                  [7 8 9]] 2)
  (neighbors [[1 2 3]
              [4 5 6]
              [7 8 9]] 2)
  (neighbors [[1 2 3]
              [4 5 6]
              [7 8 9]] 5))

(defn rotate-left [grid]
  (reverse (for [col (range (count grid))]
             (column grid col))))

(defn rotate-right [grid]
  (for [col (range (count grid))]
    (apply str (reverse (column grid col)))))

(defn flip-x [grid]
  (map (fn [row] (apply str (reverse row))) grid))

(defn flip-y [grid]
  (reverse grid))

(defn trim-edges [grid]
  (map (partial apply str)
       (map (comp rest butlast)
            (rest (butlast grid)))))

(comment
  (= ["cfi" "beh" "adg"] (rotate-left ["abc" "def" "ghi"]))
  (= ["gda" "heb" "ifc"] (rotate-right ["abc" "def" "ghi"]))
  (= ["cba" "fed" "ihg"] (flip-x ["abc" "def" "ghi"]))
  (= ["ghi" "def" "abc"] (flip-y ["abc" "def" "ghi"]))
  (= ["fg" "jk"] (trim-edges ["abcd"
                              "efgh"
                              "ijkl"
                              "mnop"])))

(defn orient-plan
  [{:keys [grid] :as tile} links neighbors]
  (into #{}
        (for [[expected-dir expected-edge]
              (set/rename-keys links (set/map-invert neighbors))
              [tile-dir tile-edge] (edges grid)
              :when (and (edge-match tile-edge expected-edge) (not= tile-dir expected-dir))]
          [tile-dir expected-dir])))

(defn plan->operation [plan]
  (cond
    (or (contains? plan [:north :south])
        (contains? plan [:south :north]))
    flip-y
    (or (contains? plan [:east :west])
        (contains? plan [:west :east]))
    flip-x
    (or (contains? plan [:south :east])
        (contains? plan [:east :north])
        (contains? plan [:north :west])
        (contains? plan [:west :south]))
    rotate-left
    (or (contains? plan [:east :south])
        (contains? plan [:north :east])
        (contains? plan [:west :north])
        (contains? plan [:south :west]))
    rotate-right
    :else identity))

;; TODO: use neighbors & links to rotate/flip-x/flip-y tile
(defn orient [tile links neighbors]
  (let [plan (orient-plan tile links neighbors)]
    (if (empty? plan)
      (trim-edges (:grid tile))
      (do
        (println (str (:id tile) " " (:edges tile) "\n     "
                      (set/rename-keys links (set/map-invert neighbors)) " "
                      neighbors "\n     "
                      plan))
        (recur (assoc tile :grid ((plan->operation plan) (:grid tile)))
               links neighbors)))))

(defn combine-image [tiles]
  (let [grid (edge-chain tiles)
        links (tile-links tiles)
        tile-index (into {} (map (fn [[id elems]] [id (first elems)])
                                 (group-by :id tiles)))]
    (for [row grid
          :let [combined
                (for [id row]
                  (orient (get tile-index id)
                          (get links id)
                          (neighbors grid id)))
                size (count (ffirst combined))]]
      (for [i (range size)]
        (map (fn [cell] (nth cell i)) combined))
      )))

(defn show-image [size combined]
  (->> combined
       flatten
       (partition size)
       ;; (map (partial interpose " "))
       (map (partial apply str))
       (map (fn [s] (str/replace s "." " ")))))

(def monster ["                  # "
              "#    ##    ##    ###"
              " #  #  #  #  #  #   "])

(def monster-regex (map (fn [s] (str/replace s " " ".")) monster))

(defn re-positions [re s]
  (let [m (re-matcher (re-pattern re) s)]
    ((fn step []
       (when (.find m)
         (cons [(.start m) (re-groups m)]
               (lazy-seq (step))))))))

(comment (re-positions #".S" "  S S      s  S"))

(defn monster-matches [n row]
  (re-positions (nth monster-regex n) row))

(defn rough-find-monsters [image]
  (->> image
       rest
       butlast ;; can't use first and last row to match from middle
       (keep-indexed (fn [idx row]
                       (when-let [m (monster-matches 1 row)]
                         [(inc idx) m])))))

(defn find-monsters [image]
  (let [middle-hits (rough-find-monsters image)]
    (flatten
     (for [[line-number hits] middle-hits
           :let [lower-row (nth image (inc line-number))
                 upper-row (nth image (dec line-number))
                 lower-hits (into {} (monster-matches 2 lower-row))
                 upper-hits (into {}
                                  (for [[offset m] hits
                                        :let [size (count m)
                                              match (subs upper-row offset (+ offset size))]
                                        :when (= \# (nth match (- size 2)))]
                                    [offset match]))]
           :when (and (seq lower-hits)
                      (seq upper-hits))]
       (for [[offset match] hits
             :let [upper (get upper-hits offset)
                   lower (get lower-hits offset)]
             :when (and lower upper)]
         {:upper-left [line-number offset]
          :r0 upper
          :r1 match
          :r2 lower})))))

(defn scan-check [monster-row region-row]
  (every? (fn [idx]
            (if (= \# (nth monster-row idx))
              (= \# (nth region-row idx))
              true))
          (range (count region-row))))

(comment (scan-check (first monster) (first monster))
         (scan-check (first monster) (second monster))
         (scan-check (first monster) (apply str (repeat 20 " "))))

(defn scan-monsters [image]
  (let [size (count image)
        monster-width (count (first monster))]
    (for [y (range (inc (- size 2)))
          x (range (inc (- size monster-width)))
          :when (every? (fn [offset]
                          (scan-check (nth monster offset)
                                      (subs (nth image (+ y offset))
                                            x (+ x monster-width))))
                        [0 1 2])]
      [x y])))

(defn count-rough-water [xs]
  (get (frequencies (apply str xs)) \#))

(defn count-rough-water-in-habitat [image]
  (- (count-rough-water image)
     (* (count (scan-monsters image))
        (count-rough-water monster))))

(comment
  (match-all-tiles (tiles "example"))
  (match-all-tiles (tiles "input"))

  (find-corners (tiles "example"))
  (find-edges (tiles "example"))

  (= 20899048083289 (first-star (tiles "example")))
  (= 54755174472007 (first-star (tiles "input")))

  (first (find-corners (tiles "input")))
  (find-edges (tiles "input"))

  (edge-chain (tiles "example"))
  (edge-chain (tiles "input"))

  (tile-links (tiles "example"))

  ;; Builds the grid but still need to
  ;; * orient tiles
  ;; * strip the edges of each element
  ;; * do a convolution match for the "sea monster" pattern
  ;; * count the unmatched coordinates in region
  (def example (flip-y (rotate-right (show-image 3 (combine-image (tiles "example"))))))
  (def input (flip-y (rotate-right (show-image 12 (combine-image (tiles "input"))))))

  ;; can also use some/juxt to look for first transformation that matches
  (some (comp seq scan-monsters)
        ((juxt flip-y flip-x rotate-right rotate-left
               (comp flip-y rotate-right) (comp flip-x rotate-right)
               (comp flip-x rotate-left) (comp flip-y rotate-left))
         (show-image 3 (combine-image (tiles "example")))))

  (find-monsters example)
  (find-monsters input)

  (= 2 (count (scan-monsters example)))
  (= 15 (count (scan-monsters input)))

  (= 15 (count (rough-find-monsters input)))

  (= \# (nth (first monster) 18))

  (= 273 (count-rough-water-in-habitat example))
  (= 1692 (count-rough-water-in-habitat input))

  (map count-pixels
       (vals (edges ["..##.#..#."
                     "##..#....."
                     "#...##..#."
                     "####.#...#"
                     "##.##.###."
                     "##...#.###"
                     ".#.#.#..##"
                     "..#....#.."
                     "###...#.#."
                     "..###..###"]))))
