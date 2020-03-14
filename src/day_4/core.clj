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

(def time-parser
  (doto (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm")
    (.setTimeZone (java.util.TimeZone/getTimeZone "GMT"))))

(defn string->instant
  [s]
  (.toInstant (.parse time-parser s)))

(defn schedule->shifts
  "Parse LINES from some schedule into shifts."
  [lines]
  (loop [lines lines
         shift nil
         nap nil
         result []]
    (let [[guard sleep awake] (which (or (first lines) "fnord"))]
      (cond guard (let [[_ time id] guard]
                    (recur (rest lines)
                           {:id id :time (string->instant time) :naps []}
                           nil
                           (conj result shift)))
            sleep (let [[_ time] sleep]
                    (recur (rest lines)
                           shift
                           {:sleep (string->instant time)}
                           result))
            awake (let [[_ time] awake
                        zzz (assoc nap :awake (string->instant time))]
                    (recur (rest lines)
                           (update shift :naps conj zzz)
                           nil
                           result))
            :else (rest result)))))

(def shifts
  (schedule->shifts input))

(first shifts)
;; => {:id "1439",
;;     :time #object[java.time.Instant 0x456018c6 "1518-02-18T00:00:00Z"],
;;     :naps [{:sleep
;;             #object[java.time.Instant 0x5db928b2 "1518-02-18T00:06:00Z"],
;;             :awake
;;             #object[java.time.Instant 0x7ccdbf5f "1518-02-18T00:28:00Z"]}]}

(defn minutes-between
  "The number of minutes between EARLIER and LATER."
  [earlier later]
  (.between java.time.temporal.ChronoUnit/MINUTES earlier later))

((juxt :sleep :awake) (first (:naps (first shifts))))
;; => [#object[java.time.Instant 0x5db928b2 "1518-02-18T00:06:00Z"]
;;     #object[java.time.Instant 0x7ccdbf5f "1518-02-18T00:28:00Z"]]

(apply minutes-between
       ((juxt :sleep :awake) (first (:naps (first shifts)))))
;; => 22
