#!/usr/bin/env bb

(require '[clojure.string :as str])

(defn parse [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map parse-long)
       (partition-by nil?)
       (take-nth 2)
       (map #(apply + %))))

(defn silver [elfs]
  (apply max elfs))

(defn gold [elfs]
  (->> elfs
       (sort)
       (reverse)
       (take 3)
       (apply +)))

(let [[file] *command-line-args*]
  (when (empty? file)
    (println "Usage: <file>")
    (System/exit 1))
  (let [elfs (parse file)]
    (printf "Silver:\t%10d\n" (silver elfs))
    (printf "Gold:\t%10d\n" (gold elfs))))
