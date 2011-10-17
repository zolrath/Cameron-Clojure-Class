(ns cameron.piglatin
  (:require [clojure.string :as str]))

(defn rotate [word]
  "Finds all non-vowel characters leading up to first occurence of vowel, drops
   that many characters from the string, and appends the match to the end.
   Clojure finds Cl, drops 2 from original string, ojure+Cl = ojureCl"
  (let [pre-vowel  (re-find #"(?i)^[^aeiou]+(?=[aeiou])|^[^y]+" word)
        post-vowel (apply str (drop (count pre-vowel) word))
        rotated    (str post-vowel pre-vowel)]
    rotated))

(defn capitalize? [word]
  "Returns proper capitalization function depending on if word starts with a
   capital or is entirely capitals."
  (cond
   (re-find #"^[^a-z]*$" word) str/upper-case
   (re-find #"^[A-Z][^A-Z]*$" word) str/capitalize
   :else identity))

(defn start-punctuation? [word]
  "Returns string containing all non-alphanumeric characters, aside from
   single quotation mark('), before word."
  (re-find #"(?i)^[^\w']+" word))

(defn end-punctuation? [word]
  "Returns string containing all non-alphanumeric characters, aside from
   single quotation mark('),  after word."
  (re-find #"(?i)[^\w']+$" word))

(defn remove-punctuation [word]
  "Removes all non-alphanumeric characters, aside from single quotation
   mark ('), from word."
  (str/replace word #"(?i)[^\w']+" ""))

(defn latin [word]
  "Return pig latin version of single word."
  (let [s-punc (start-punctuation? word)
        e-punc (end-punctuation? word)
        word   (remove-punctuation word)
        vowel? (set "aeiouAEIOU")
        caps?  (capitalize? word)]
    (if (vowel? (first word))
      (caps? (str s-punc word "way" e-punc))
      (caps? (str s-punc (rotate word) "ay" e-punc)))))

(declare latin-dispatch)

(defn latin-separate [separator word]
  "Split word on separator and return pig latin version of each word."
  (let [re-separator (str "\\" separator)
        end (if (= separator (str (last word))) separator)]
    (if (= separator (str (first word)))
      (str separator (latin-dispatch (apply str (rest word))))
      (str (str/join separator (map latin-dispatch (str/split word (re-pattern re-separator)))) end))))

(defn latin-dispatch [word]
  "If word contins digit, return unmodified. If word has non-alpha or ' character
   split on given character."
  (let [no-vowels  (nil? (re-find #"(?i)[aeiouy]" word))
        has-digits (re-find #"^\d+" word)
        separator  (re-find #"(?i)[^a-z](?=[a-z])" word)]
    (cond
       no-vowels word
       has-digits word
       separator (latin-separate separator word)
       :else (latin word))))

(defn pig-sentence [sentence]
  "Map pig latin functions onto each word in a sentence."
  (let [split-sent (str/split sentence #"\s+")]
    (str/join " " (map latin-dispatch split-sent))))

(defn translate []
  "Endless loop so you can see results instantly. Enter q to quit."
  (println "Let the translating begin! Type q to end.")
  (loop []
    (let [text (read-line)]
      (if (not= "q" text)
        (do (println (pig-sentence text))
            (recur))))))
