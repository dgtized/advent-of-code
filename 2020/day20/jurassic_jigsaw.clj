(ns jurassic-jigsaw
  (:require [clojure.string :as str]))

(defn column [grid col]
  (str/join (map #(nth % col) grid)))

(defn edges [grid]
  [(nth grid 0) ;; north
   (column grid 9) ;; east
   (nth grid 9) ;; south
   (column grid 0) ;; west
   ])

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
          (for [edge1 (:edges tile1)
                edge2 (:edges tile2)
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
                {:north [-1 0] :east [0 1] :south [1 0] :west [0 -1]}
                :let [coord (map + neighbor position)
                      value (get-in grid coord)]
                :when value]
            [direction value]))))

(comment
  (find-position [[1 2 3]
                  [4 5 6]
                  [7 8 9]] 9)
  (neighbors [[1 2 3]
              [4 5 6]
              [7 8 9]] 2))

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

;; TODO: use neighbors & links to rotate/flip-x/flip-y tile
(defn orient [tile links neighbors]
  (println (str (:id tile) " " links " " neighbors))
  (:grid tile))

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
  (combine-image (tiles "example"))
  (combine-image (tiles "input"))

  (map count-pixels
       (edges ["..##.#..#."
               "##..#....."
               "#...##..#."
               "####.#...#"
               "##.##.###."
               "##...#.###"
               ".#.#.#..##"
               "..#....#.."
               "###...#.#."
               "..###..###"]))
  )
