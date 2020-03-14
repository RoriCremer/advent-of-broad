(ns day_4
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def input-txt (slurp "./src/day_4/input.txt"))

(def input (-> input-txt (str/split #"\n") sort))

(def guard-re #"^\[(.+)\]\s+Guard\s+#(\d+)\s+begins\s+shift\s*$")
(def sleep-re #"^\[(.+)\]\s+falls\s+asleep\s*$")
(def awake-re #"^\[(.+)\]\s+wakes\s+up\s*$")

(def guard (partial re-find guard-re))
(def sleep (partial re-find sleep-re))
(def awake (partial re-find awake-re))
(def which (juxt guard sleep awake))

(guard (nth input 0))
;; => ["[1518-02-08 00:00] Guard #1439 begins shift"
;;     "1518-02-08 00:00"
;;     "1439"]

(sleep (nth input 1))
;; => ["[1518-02-08 00:06] falls asleep" "1518-02-08 00:06"]

(awake (nth input 2))
;; => ["[1518-02-08 00:28] wakes up" "1518-02-08 00:28"]

(defn schedule->shifts
  [lines]
  (loop [lines lines
         shift nil
         nap nil
         result []]
    (let [[guard sleep awake] (which (or (first lines) "fnord"))]
      (cond guard (let [[_ time id] guard]
                    (recur (rest lines)
                           {:id id :time time :naps []}
                           nil
                           (conj result shift)))
            sleep (let [[_ time] sleep]
                    (recur (rest lines)
                           shift
                           {:sleep time}
                           result))
            awake (let [[_ time] awake
                        nap (assoc nap :awake time)]
                    (recur (rest lines)
                           (update shift :naps conj nap)
                           nil
                           result))
            :else (rest result)))))

(def shifts
  (schedule->shifts input))
