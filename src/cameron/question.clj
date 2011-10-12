(ns cameron.question
  "Simple prompt to enter data into CSV file to track daily stuffs."
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:use     [date-clj]))

(defn add-headers? [filename headers]
  "Checks to see if file exists. If not, add each element of headers vector to
   initialize the file."
  (let [filecsv (io/file (str filename ".csv"))]
    (if (false? (.exists filecsv))
      (with-open [file (io/writer filecsv)]
        (spit file (str/join "," (conj headers \newline)))))))

(defn add-to-file [filename data]
  "Appends all elements in data vector to a CSV file of given name."
  (let [filecsv (io/file (str filename ".csv"))]
    (with-open [file (io/writer filecsv :append true)]
      (spit file (str/join "," (conj data \newline))))))

(defn no-comma [input]
  "Replaces all , in input with ; to maintain CSV ordering."
  (str/replace input "," ";"))

(defn this-or-that [[this that]]
  "Takes a vector with two choices (strings) and only accepts one of them as the answer.."
  (let [answer (str/lower-case (read-line))]
    (cond
     (= answer (str/lower-case this)) (no-comma this)
     (= answer (str/lower-case that)) (no-comma that)
     :else (do (println (str "Please answer " this " or " that ".")) (recur [this that])))))

(defn y-or-n []
  "Return true on y__, false on n__. On most any other answer, prompt for y or n.
   Will Smith Support added."
  (let [answer (str/lower-case (read-line))]
    (cond
     (= (re-find #"^y" answer) "y") true
     (= (re-find #"^n" answer) "n") false
     (= answer "aw hell naw!") false
     :else (do (println "Please answer y or n.") (recur)))))

(defn input-scale [[min max]]
  "Takes a vector of minimum and maximum values (integers) of input."
  (let [answer (try (Integer/parseInt (read-line)) (catch Exception e (dec min)))]
    (if (and (>= answer min) (<= answer max))
      answer
      (do (println (str "Please enter an integer between " min " and " max "."))
          (recur [min max])))))

(defn input-date []
  "Requires that the date be entered in a valid and standardized format.
   If user enters the word today it inputs todays date."
  (let [answer (read-line)
        date   (re-find #"(\d{2})/(\d{2})/\d{4}" answer)
        month  (read-string (or (nth date 1) "0"))
        day    (read-string (or (nth date 2) "0"))]
    (cond
     (= answer "today") (format-date (today) "MM/dd/yyyy")
     (and (>= month 1) (<= month 12) (>= day 1) (<= day 31)) (first date)
     :else (do (println "Please input the date in the format of MM/DD/YYYY (e.g. 01/25/1986) or enter \"today\"")
            (recur)))))

(defn ask-category [prompt]
  "Asks if you participated in given category. Bind result to var for use in get-input."
  (println (str prompt " (y/n)"))
  (y-or-n))

(defn get-input [category prompt & [validator arg-vec]]
  "Bind result of ask-category to a var and use it as the category field to group questions.
   If the question should be asked regardless of any particular category answer, set category to true.
   Can be passed optional keyword (and sometimes vector of arguments) to determine input type.
   :y-or-n makes the question only accept y or n.
   :this-or-that only accepts the strings you put in the vector. (e.g. ['Man' 'Machine'])
   :scale only accepts answers between min and max in supplied vector. (e.g. [1 10])
   :date only accepts numbers in format of MM/DD/YYYY"
  (if category
    (do (println prompt)
        (case validator
          :y-or-n (y-or-n)
          :this-or-that (this-or-that arg-vec)
          :scale (input-scale arg-vec)
          :date (input-date)
          nil (no-comma (read-line))))))

(defn prompt []
  "Add data to CSV file as long as user answers y or yes to Add another?
   To add additional fields be sure that the order of the headers and the
   order of the variables in add-to-file are the same. Headers are only added
   when file doesn't exist."
  (add-headers? "data" ["Date" "Mood" "Miles Driven"
                        "Books Name" "Number of Pages"
                        "Movie Seen" "Home/Theater" "Movie Seen Alone?"
                        "Board Game Name" "Time Spent Playing"])
  (loop []
    (let [date       (get-input true "What day are we talking about? (MM/DD/YYYY or today)" :date)
          mood       (get-input true "How are/were you feeling? (Scale of 1-10)" :scale [1 10])
          miles      (get-input true "How many miles did you drive?")

          reading    (ask-category "Did you read any books?")
          bookname   (get-input reading "What was the books title?")
          pagenum    (get-input reading "How many pages did you read?")

          movies     (ask-category "Did you watch a movie?")
          movietitle (get-input movies "What movie was it?")
          movieloc   (get-input movies "Did you see it at Home or at the Theater?" :this-or-that ["Home" "Theater"])
          moviealone (get-input movies "Did you watch it alone? (y/n)" :y-or-n)

          boardgames (ask-category "Did you play any board games?")
          bgamename  (get-input boardgames "What boardgame did you play?")
          bgametime  (get-input boardgames "How long did it take to play? (in minutes)")]
      (add-to-file "data" [date mood miles
                           bookname pagenum
                           movietitle movieloc moviealone
                           bgamename bgametime])
      (println "Add another? (y/n)")
      (if (y-or-n)
        (recur)
        (println "No more entries? Go forth and do more stuff then hurry back and tell me. GO!")))))
