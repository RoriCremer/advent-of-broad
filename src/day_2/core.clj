(ns day-2.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]))

(defmacro dump
  "Dump [EXPRESSION VALUE] where VALUE is EXPRESSION's value."
  [expression]
  `(let [x# ~expression]
     (do
       (pprint ['~expression x#])
       x#)))

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

(def word "ymdrcyapvwfloiuktanxzjsieb")
(defn hole [n] (str (subs word 0 n) "_" (subs word (+ n 1))))
(hole 0)
(hole 25)
'(hole 26)

(defn holify [word]
  (letfn [(hole [n] (str (subs word 0 n) "_" (subs word (+ n 1))))]
    (loop [result [] n (- (count word) 1)]
      (if (zero? n)
        (conj result (hole 0))
        (recur (conj result (hole n)) (- n 1))))))

(def hole-seq (mapcat holify input))
(def hole-set (set (mapcat holify input)))

(dump [(count hole-seq) (count hole-set)])

(defn solve
  [holified]
  (loop [words holified seen #{}]
    (when-let [w (first words)]
      (if (seen w)
        (str/replace w "_" "")
        (recur (rest words) (conj seen w))))))

(solve ["a" "a"])
(solve ["a" "z"])

(solve hole-seq)
