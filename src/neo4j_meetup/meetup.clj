(ns neo4j-meetup.meetup
  (:require [clj-http.client :as client])
  (:require [clojure.data.json :as json])
  (:require [environ.core :as e])
  (:require [clj-time.core :as t])
  (:require [clj-time.coerce :as c])
  (:require [clj-time.format :as f])
  (:require [neo4j-meetup.db :as db]
            [neo4j-meetup.timestamp :as timestamp])
  (:require [clojurewerkz.neocons.rest :as nr]
            [clojurewerkz.neocons.rest.transaction :as tx]))

(defn day-suffix [day]
  (let [stripped-day (if (< day 20) day (mod day 10))]
    (cond (= stripped-day 1) "st"
          (= stripped-day 2) "nd"
          (= stripped-day 3) "rd"
          :else "th")))

(defn extract-date-time [timestamp]
  { :formatted-time (timestamp/as-time timestamp)
    :formatted-date (timestamp/as-date timestamp) })

(defn all-events [meetup-name]
  (let [query "MATCH (event:Event)
               RETURN event"]
    (->>
     (db/tx-api-single query)
     :data
     (map #(reduce merge (:row %)))
     (map #(merge % (extract-date-time (+ (:time %) (:utc_offset %))))) )))

(defn event [event-id]
  (let [query "MATCH (event:Event {id: {eventId}})-[:HELD_AT]->(venue)
               MATCH (event)<-[:TO]-(rsvp)<-[:RSVPD]-(person)
               WITH event, venue, rsvp, person
               ORDER BY rsvp.time
               OPTIONAL MATCH (rsvp)<-[:NEXT]-(initial)
               WITH event,
                    venue,
                    COLLECT({rsvp: rsvp, initial: initial, person: person}) AS responses
               RETURN event,
                      venue,
                      [response in responses WHERE response.initial is null
                                             AND response.rsvp.response = 'yes'] as attendees,
                      [response in responses WHERE not response.initial is null] as dropouts
"
        params {:eventId event-id}]
    (->>
     (db/cypher query params)
     (map #(merge %  (extract-date-time
                      (+ (-> % :event :data :time) (-> % :event :data :utc_offset)))))
     first)))
