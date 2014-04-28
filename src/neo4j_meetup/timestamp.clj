(ns neo4j-meetup.timestamp
  (:require [clj-http.client :as client])
  (:require [clojure.data.json :as json])
  (:require [environ.core :as e])
  (:require [clj-time.core :as t])
  (:require [clj-time.coerce :as c])
  (:require [clj-time.format :as f]))

(defn as-time [timestamp]
  (let [time (c/from-long timestamp)]
    (f/unparse (f/formatter "HH:mm") time)))

(defn day-suffix [day]
  (let [stripped-day (if (< day 20) day (mod day 10))]
    (cond (= stripped-day 1) "st"
          (= stripped-day 2) "nd"
          (= stripped-day 3) "rd"
          :else "th")))

(defn as-date [timestamp]
  (let [time (c/from-long timestamp)
        day (read-string (f/unparse (f/formatter "d") time))]
    (str day
         (day-suffix day)
         " "
         (f/unparse (f/formatter "MMMM yyyy") time)) ))

(defn as-date-time [timestamp]
  (let [time (c/from-long timestamp)
        day (read-string (f/unparse (f/formatter "d") time))]
    (str day
         (day-suffix day)
         " "
         (f/unparse (f/formatter "MMMM yyyy, HH:mm") time))))
