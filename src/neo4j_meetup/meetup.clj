
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

(defn day-suffix [day]
  (let [stripped-day (if (< day 20) day (mod day 10))]
    (cond (= stripped-day 1) "st"
          (= stripped-day 2) "nd"
          (= stripped-day 3) "rd"
          :else "th")))

(defn extract-date-time [timestamp]
  (let [time (c/from-long timestamp)
        day (read-string (f/unparse (f/formatter "d") time))]
    { :formatted-time
      (f/unparse (f/formatter "HH:mm") time)
      :formatted-date
      (str day (day-suffix day) " " (f/unparse (f/formatter "MMMM yyyy") time)) }))

(defn all-events [meetup-name]
  (let [query "MATCH (event:Event)
               RETURN event"]
    (->>
     (db/tx-api-single query)
     :data
     (map #(reduce merge (:row %)))
     (map #(merge % (extract-date-time (+ (:time %) (:utc_offset %))))) )))

(comment (defn event [event-id]
           (let [query "MATCH (event:Event {id: {eventId}})-[:HELD_AT]->(venue)
               RETURN event, venue"
                 params {:eventId event-id}]
             (->>
              (db/tx-api-single query params)
              :data
              (map #(reduce merge (:row %)))
              (map #(merge % (extract-date-time (+ (:time %) (:utc_offset %))))) 
              first))))

(defn event [event-id]
  (let [query "MATCH (event:Event {id: {eventId}})-[:HELD_AT]->(venue)
MATCH (event)<-[:TO]-(rsvp)<-[:RSVPD|:INITIALLY_RSVPD]-(person)
WITH event, venue, rsvp, person
ORDER BY rsvp.time
RETURN event, venue, COLLECT({rsvp: rsvp, person: person}) AS responses"
        params {:eventId event-id}]
    (->>
     (db/cypher query params)
     (map #(merge %  (extract-date-time
                      (+ (-> % :event :data :time) (-> % :event :data :utc_offset)))))
     first)))


