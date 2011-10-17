(ns cameron.better-piglatin
  (:require [clojure.string :as str]))

(defn rotate [word]
  "Finds all non-vowel characters leading up to first occurence of vowel,
   appends that to the end of remaining string.
   Clojure =>  ojure+Cl = ojureCl"
  (let [pre-vowel  (re-find #"(?i)^[^aeiou]+(?=[aeiou])|^[^y]+" word)
        post-vowel (re-find #"(?i)[aeiou].+|[y].+" word)]
    (str post-vowel pre-vowel)))

(defn capitalize? [word]
  "Returns proper capitalization function depending on if word starts with a capital or is entirely capitals."
  (cond
   (re-find #"^[^a-z]*$" word) str/upper-case
   (re-find #"^[A-Z][^A-Z]*$" word) str/capitalize
   :else identity))

(defn word-to-pig-latin [word]
  "Translates given word into Pig Latin."
  (let [vowel? (set "aeiouAEIOU")
        caps? (capitalize? word)]
    (if (vowel? (first word))
      (caps? (str word "way"))
      (caps? (str (rotate word) "ay")))))

(defn pig-sentence [sentence]
  "Takes sentence and replaces only alpha characters and ' in place with its pig-latin translation."
  (let [words (set (re-seq #"(?i)(?<=')[a-z]+(?=')|[a-z]+'?[a-z]*" sentence))]
    (reduce #(str/replace % %2 (word-to-pig-latin %2)) sentence words)))

(defn translate []
  "Endless loop so you can see results instantly. Enter q to quit."
  (println "Let the translating begin! Type q to end.")
  (loop []
    (let [text (read-line)]
      (if (not= "q" text)
        (do (println (pig-sentence text))
            (recur))))))
