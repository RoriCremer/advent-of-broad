(ns day-2.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(.getCanonicalPath (io/file "."))

(def input (str/split (slurp "./src/day_2/input.txt")  #"\n"))

(defn count-letters [text]
  (loop [counts {} text text]
    (if (empty? text)
      counts
      (when-let [letter (first text)]
        (recur (assoc counts letter (inc (get counts letter 0)))
               (rest text))))))

(count-letters "wnlngevmfodamyiqpuzcrhstkg")

(frequencies (keep #{2 3} (set(vals (frequencies "wnlngevmfodamyiqpuzcrhstkg")))))

(def find-frequencies (fn [word] (set (vals (frequencies word)))))

(def freeks (frequencies (mapcat find-frequencies input)))

(freeks 2)
(freeks 3)

(* (freeks 2) (freeks 3))

(apply * (vals (select-keys freeks [2 3])))

(defn hole [])

(defn holify [word]
  (loop [result [] n (count word)]
    (if (zero? n)
      (conj result (hole 0 word))
      "Now what?")))
