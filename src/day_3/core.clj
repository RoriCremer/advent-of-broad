(ns day_3
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defmacro make-map
  "Map SYMBOLS as keywords to their values in the environment."
  [& symbols]
  (zipmap (map keyword symbols) symbols))

(def line
  "Sample line from the input.txt file."
  "#1 @ 7,589: 24x11")

(def re
  "Regular expression to parse a fabric from a line."
  #"#(\d+)\s@\s(\d+),(\d+):\s(\d+)x(\d+)")

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
  (-> file
      slurp
      (str/split #"\n")
      (->> (map parse-fabric))))

(first (parse-input "./src/day_3/input.txt"))
;; => {:id 1, :x 7, :y 589, :Δx 24, :Δy 11}

(defn add-fabric
  "Add FABRIC to GRID."
  [grid {:keys [x Δx y Δy] :as fabric}]
  (apply merge
         (for [x (range x (+ x Δx))
               y (range y (+ y Δy))]
           (assoc grid [x y] ((fnil inc 0) (grid [x y]))))))

(add-fabric {} fabric)
