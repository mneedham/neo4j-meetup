(ns neo4j-meetup.meetup
  (:require [clj-http.client :as client])
  (:require [clojure.data.json :as json])
  (:require [environ.core :as e])
  (:require [clj-time.core :as t])
  (:require [clj-time.coerce :as c])
  (:require [clj-time.format :as f])
  (:require [neo4j-meetup.db :as db])
  (:require [clojurewerkz.neocons.rest :as nr]
            [clojurewerkz.neocons.rest.transaction :as tx]))

(defn extract-date-time [timestamp]
  (let [time (c/from-long timestamp)
        day (read-string (f/unparse (f/formatter "d") time))]
    {
     :formatted-time
     (f/unparse (f/formatter "HH:mm") time)
     :formatted-date
     (str day (day-suffix day) " "
          (f/unparse (f/formatter "MMMM yyyy") time))  
     }))

(defn day-suffix [day]
  (let [stripped-day (if (< day 20) day (mod day 10))]
    (cond (= stripped-day 1) "st"
          (= stripped-day 2) "nd"
          (= stripped-day 3) "rd"
          :else "th")))

(defn all-events [meetup-name]
  (let [query "MATCH (event:Event)
               RETURN event"]
    (->>
     (db/tx-api-single query)
     :data
     (map #(reduce merge (:row %)))
     (map #(merge % (extract-date-time (+ (:time %) (:utc_offset %))))) )))
