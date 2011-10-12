(ns cameron.question
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn add-headers? [filename headers]
  "Checks to see if file exists, if not, add each element of headers vector."
  (if (false? (.exists (io/file (str filename ".csv"))))
    (with-open [file (io/writer (io/file (str filename ".csv")))]
      (spit file (str/join "," (conj headers \newline))))))

(defn add-to-file [filename data]
  "Appends all elements in a given vector of data to a CSV file of given name."
  (with-open [file (io/writer (io/file (str filename ".csv"))
                              :append true)]
    (spit file (str/join "," (conj data \newline)))))

(defn get-input [prompt]
  "Prints out prompt and returns user input."
  (println prompt)
  (read-line))

(defn prompt []
  "Add data to CSV file as long as user answers y or yes to Add another?"
  (add-headers? "data" ["Name" "Age" "Sex" "Has Monkey?"])
  (loop []
    (let [name   (get-input "Who we talkin' about here? ")
          age    (get-input "How old is the bum? ")
          sex    (get-input "Are they sexy? Errr... Male or Female?")
          monkey (get-input "Do they have a monkey? That funky monkey. Brass Monkey. ")]
      (add-to-file "data" [name age sex monkey])
      (if (= (get-input "Add another? (y or yes)") (or "y" "yes" "Y" "Yes"))
        (recur)))))
