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

(defn tiles-with-n-links [tiles n]
  (let [matches (match-all-tiles tiles)]
    (->> matches
         (group-by first)
         (map (fn [[k v]] [k (mapv rest v)]))
         (filter (fn [[k v]] (= n (count v)))))))

(defn find-corners [tiles]
  (tiles-with-n-links tiles 2))

(defn find-edges [tiles]
  (tiles-with-n-links tiles 3))

(defn first-star [tiles]
  (let [corner-ids (map first (find-corners tiles))]
    (apply * corner-ids)))

(comment
  (match-all-tiles (tiles "example"))
  (match-all-tiles (tiles "input"))

  (find-corners (tiles "example"))
  (find-edges (tiles "example"))

  (= 20899048083289 (first-star (tiles "example")))
  (= 54755174472007 (first-star (tiles "input")))

  (find-corners (tiles "input"))
  (find-edges (tiles "input"))

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
