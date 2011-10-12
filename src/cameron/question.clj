(ns cameron.question
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn add-to-file [[name age sex monkey]]
  "Appends Name,Age,Sex,Has-Monkey to CSV file."
  (with-open [file (io/writer (io/file "data.csv")
                              :append true)]
    (spit file (str/join "," (list name age sex monkey \newline)))))

(defn get-input [prompt]
  "Prints out prompt and returns user input."
  (println prompt)
  (read-line))

(defn prompt []
  "Add data to CSV file as long as user answers y or yes to Add another?"
  (loop []
    (let [name   (get-input "Who we talkin' about here? ")
          age    (get-input "How old is the bum? ")
          sex    (get-input "Are they sexy? Errr... Male or Female?")
          monkey (get-input "Do they have a monkey? That funky monkey. Brass Monkey. ")]
      (add-to-file [name age sex monkey])
      (if (= (get-input "Add another? (y or yes") (or "y" "yes" "Y" "Yes"))
        (recur)))))
