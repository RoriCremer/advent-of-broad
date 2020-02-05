(ns day-1.core
  (:gen-class))

  (require '[clojure.string :as str])
  (.getCanonicalPath (clojure.java.io/file "."))

  (defn -main
    "I don't do a whole lot ... yet."
    [& args]
    (println "Hello, World!"))

    (def input (map read-string (str/split (slurp "./input.txt")  #"\n")))

    (reduce + 0  input)


    (loop [ac 0 input (cycle input) seen #{}]
      (let [x (first input) next-ac (+ ac x)]
      (if (seen next-ac) next-ac
      (recur next-ac (rest input) (conj seen next-ac)))))
