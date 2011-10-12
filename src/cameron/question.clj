(ns cameron.question
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn add-headers? [filename headers]
  "Checks to see if file exists, if not, add each element of headers vector."
  (let [filecsv (io/file (str filename ".csv"))]
        (if (false? (.exists filecsv))
        (with-open [file (io/writer filecsv)]
          (spit file (str/join "," (conj headers \newline)))))))

(defn add-to-file [filename data]
  "Appends all elements in a given vector of data to a CSV file of given name."
  (let [filecsv (io/file (str filename ".csv"))]
    (with-open [file (io/writer filecsv :append true)]
      (spit file (str/join "," (conj data \newline))))))

(defn get-input [category prompt]
  "Bind result of ask-category to a var and use it as the category field if the
   question must be asked regardless of category answer, set category to true."
  (if category
    (do (println prompt)
        (read-line))))

(defn y-or-n []
  "Return true on y, false on n. On any other answer, prompt for y or n."
  (let [answer (read-line)]
    (if (= answer "y") true
        (if (= answer "n") false
            (do (println "Please answer y or n") (recur))))))

(defn ask-category [prompt]
  "Asks if you participated in given category. Bind result to var for use in get-input."
  (println (str prompt " (y/n)"))
  (y-or-n))

(defn prompt []
  "Add data to CSV file as long as user answers y or yes to Add another?"
  (add-headers? "data" ["Date" "Mood" "Miles Driven"
                        "Books Name" "Number of Pages"
                        "Movie Seen" "Home/Theater" "Movie Seen Alone?"
                        "Board Game Name" "Time Spent Playing"])
  (loop []
    (let [date       (get-input true "What day are we talking about? (MM/DD/YYYY)")
          mood       (get-input true "How are you feeling today? (Scale of 1-10)")
          miles      (get-input true "How many miles did you drive today?")

          reading    (ask-category "Did you read any books today?")
          bookname   (get-input reading "What was the books title?")
          pagenum    (get-input reading "How many pages did you read?")

          movies     (ask-category "Did you watch a movie today?")
          movietitle (get-input movies "What movie was it?")
          movieloc   (get-input movies "Did you see it at Home or at the Theater?")
          moviealone (get-input movies "Did you watch it alone? (y/n)")

          boardgames (ask-category "Did you play any board games today?")
          bgamename  (get-input boardgames "What boardgame did you play?")
          bgametime  (get-input boardgames "How long did it take to play? (in minutes)")]
      (add-to-file "data" [date mood miles
                           bookname pagenum
                           movietitle movieloc moviealone
                           bgamename bgametime])
      (println "Add another? (y or n)")
      (if (y-or-n)
        (recur)))))
