(ns day_3
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defmacro make-map
  "Map SYMBOLS as keywords to their values in the environment."
  [& symbols]
  (zipmap (map keyword symbols) symbols))

(def line "#1 @ 7,589: 24x11")
(def re #"#([0-9]*) @ ([0-9]*),([0-9]*): ([0-9]*)x([0-9]*)")

(defn parse-fabric
  "Parse LINE into a fabric map."
  [line]
  (let [[id x y x-delta y-delta]
        (->> line
             (re-find re)
             rest
             (map edn/read-string))]
    (make-map id x y x-delta y-delta)))

(def fabric (parse-fabric line))
;; => {:id 1, :x 7, :y 589, :x-delta 24, :y-delta 11}

(defn parse-input
  "Parse input file into a seq of fabric maps."
  [file]
  (-> file
      slurp
      (str/split #"\n")
      (->> (map parse-fabric))))

(parse-input "./src/day_3/input.txt")

(defn add-fabric
  "Add FABRIC to GRID."
  [grid {:keys [x x-delta y y-delta] :as fabric}]
  (apply merge
         (for [x (range x (+ x x-delta))
               y (range y (+ y y-delta))]
           (assoc grid [x y] ((fnil inc 0) (grid [x y]))))))

(add-fabric {} {:id 1, :x 7, :y 589, :x-delta 24, :y-delta 11})
