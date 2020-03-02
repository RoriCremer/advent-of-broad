(ns day_3
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defmacro make-map
  "Map SYMBOLS as keywords to their values in the environment."
  [& symbols]
  (zipmap (map keyword symbols) symbols))

(def line
  "Sample line from the input.txt file."
  (do
    "id   x   y  Δx Δy"
    "#1 @ 7,589: 24x11"))

(def re
  "Regular expression to parse a fabric from a line."
  (do
    "    id         x     y      Δx    Δy"
    #"#(\d+)\s@\s(\d+),(\d+):\s(\d+)x(\d+)"))

(defn parse-fabric
  "Parse LINE into a fabric map."
  [line]
  (let [[id x y Δx Δy]
        (->> line
             (re-find re)
             rest
             (map edn/read-string))]
    (make-map id x y Δx Δy)))

(def fabric
  "A sample fabric map."
  (parse-fabric line))
;; => {:id 1, :x 7, :y 589, :Δx 24, :Δy 11}

(defn parse-input
  "Parse input file into a seq of fabric maps."
  [file]
  (-> file slurp (str/split #"\n")
      (->> (map parse-fabric))))

(first (parse-input "./src/day_3/input.txt"))
;; => {:id 1, :x 7, :y 589, :Δx 24, :Δy 11}

(def the-fabrics (parse-input "./src/day_3/input.txt"))

(count the-fabrics)

;; Nick's idea.
;;
(defn fabric->coords
  "The sequence of coordinates of 'square inches' of FABRIC."
  [{:keys [x Δx y Δy] :as fabric}]
  (for [x (range x (+ x Δx))
        y (range y (+ y Δy))]
    [x y]))

(def grid
  "Map of coordinates to the fabric count overlapping there."
  (frequencies (mapcat fabric->coords the-fabrics)))

(def square-inches-of-fabric-overlap
  "1st Answer: How many square inches of fabric overlap?"
  (count (filter (fn [n] (> n 1)) (vals grid))))

square-inches-of-fabric-overlap         ; => 117948

(defn non-overlapping-fabric?
  "Nil or the ID of a fabric when it doesn't overlap."
  [{:keys [id x Δx y Δy] :as fabric}]
  (when (every? (fn [xy] (== 1 (grid xy)))
                (fabric->coords fabric))
    id))

(def ids-of-fabrics-that-do-not-overlap
  "2nd Answer: Find fabrics that do not overlap."
  (remove nil? (map non-overlapping-fabric? the-fabrics)))

ids-of-fabrics-that-do-not-overlap      ; => (567)
