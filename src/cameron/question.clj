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

(defn this-or-that [[this that]]
  "Takes a vector of two choices and will only allow accept either choice as the answer."
  (let [answer (read-line)]
    (if (= (.toLowerCase answer) (.toLowerCase this)) this
        (if (= (.toLowerCase answer) (.toLowerCase that)) that
            (do (println (str "Please answer " this " or " that)) (recur [this that]))))))

(defn y-or-n []
  "Return true on y, false on n. On any other answer, prompt for y or n."
  (let [answer (read-line)]
    (if (= answer "y") true
        (if (= answer "n") false
            (do (println "Please answer y or n") (recur))))))

(defn input-scale [[min max]]
  "Takes a vector of minimum and maximum values of input."
  (let [answer (Integer/parseInt (read-line))]
    (if (and (>= answer min) (<= answer max))
      answer
      (do (println (str "Please answer between " min " and " max))
          (recur [min max])))))

(defn input-date []
  "Requires that the date be entered in a valid and standardized format."
  (let [answer (read-line)
        date   (re-find #"(\d{2})/(\d{2})/\d{4}" answer)
        month  (Integer/parseInt (or (nth date 1) "0"))
        day    (Integer/parseInt (or (nth date 2) "0"))]
    (if (and (> month 0) (< month 13) (> day 0) (< day 32))
      answer
      (do (println "Please input the date in the format of MM/DD/YYYY (e.g. 01/25/1986)")
          (recur)))))

(defn ask-category [prompt]
  "Asks if you participated in given category. Bind result to var for use in get-input."
  (println (str prompt " (y/n)"))
  (y-or-n))

(defn get-input [category prompt & validation]
  "Bind result of ask-category to a var and use it as the category field if the
   question must be asked regardless of category answer, set category to true.
   Can be passed optional keyword to determine input type.
   :y-or-n makes the question only accept y or n.
   :this-or-that only accept varibles in given vector.
   :scale only accept answers between 1 and 10
   :date only accepts numbers in format of MM/DD/YYYY"
  (if category
    (do (println prompt)
        (case (first validation)
          :y-or-n (y-or-n)
          :this-or-that (this-or-that (first (rest validation)))
          :scale (input-scale (first (rest validation)))
          :date (input-date)
          nil (read-line)))))

(defn prompt []
  "Add data to CSV file as long as user answers y or yes to Add another?"
  (add-headers? "data" ["Date" "Mood" "Miles Driven"
                        "Books Name" "Number of Pages"
                        "Movie Seen" "Home/Theater" "Movie Seen Alone?"
                        "Board Game Name" "Time Spent Playing"])
  (loop []
    (let [date       (get-input true "What day are we talking about? (MM/DD/YYYY)" :date)
          mood       (get-input true "How are you feeling today? (Scale of 1-10)" :scale [1 10])
          miles      (get-input true "How many miles did you drive today?")

          reading    (ask-category "Did you read any books today?")
          bookname   (get-input reading "What was the books title?")
          pagenum    (get-input reading "How many pages did you read?")

          movies     (ask-category "Did you watch a movie today?")
          movietitle (get-input movies "What movie was it?")
          movieloc   (get-input movies "Did you see it at Home or at the Theater?" :this-or-that ["Home" "Theater"])
          moviealone (get-input movies "Did you watch it alone? (y/n)" :y-or-n)

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
