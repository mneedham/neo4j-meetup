(ns neo4j-meetup.core
  (:require [clj-http.client :as client])
  (:require [clojure.data.json :as json])
  (:require [environ.core :as e])
  (:require [clj-time.core :as t])
  (:require [clj-time.coerce :as c])
  (:require [neo4j-meetup.db :as db])
  (:require [clojurewerkz.neocons.rest :as nr]
            [clojurewerkz.neocons.rest.transaction :as tx]))

(def MEETUP_KEY (e/env :meetup-key))
(def MEETUP_NAME "graphdb-london")

(defn unchunk [s]
  (when (seq s)
    (lazy-seq
      (cons (first s)
            (unchunk (next s))))))

(defn offsets []
  (unchunk (range)))

(defn members
  [{perpage :perpage offset :offset orderby :orderby}]
  (->> (client/get
        (str "https://api.meetup.com/2/members?page=" perpage
             "&offset=" offset
             "&orderby=" orderby
             "&group_urlname=" MEETUP_NAME
             "&key=" MEETUP_KEY)
        {:as :json})
       :body :results))

(defn events
  [{perpage :perpage offset :offset orderby :orderby}]
  (->> (client/get
        (str "https://api.meetup.com/2/events?page=" perpage
             "&offset=" offset
             "&orderby=" orderby
             "&status=upcoming,past&"
             "&group_urlname=" MEETUP_NAME
             "&key=" MEETUP_KEY)
        {:as :json})
       :body :results))

(defn rsvps
  [event-id {perpage :perpage offset :offset orderby :orderby}]
  (let [uri (str "https://api.meetup.com/2/rsvps?page=" perpage
             "&event_id=" event-id
             "&offset=" offset
             "&orderby=" orderby
             "&key=" MEETUP_KEY)]
     (->> (client/get
           uri
           {:as :json})
          :body :results)))

(defn get-all [api-fn]
  (flatten
   (take-while seq
               (map #(api-fn {:perpage 200 :offset % :orderby "name"}) (offsets)))))

(defn all-events []
  (get-all events))

(defn all-members []
  (get-all members))

(defn save [file data]
  (clojure.core/spit file (json/write-str data)))

(defn load [file]
  (json/read-str (slurp file) :key-fn keyword))

(defn link-credo-venues []
  (db/tx-api-single "MATCH (v1:Venue {id: 9695352})
                  MATCH (v2:Venue {id: 10185422})
                  MERGE (v1)-[:ALIAS_OF]->(v2)"))

(defn create-time-tree [start-year end-year]
  (db/tx-api-single "
    WITH range({start}, {end}) AS years, range(1,12) as months
    FOREACH(year IN years | 
      MERGE (y:Year {year: year})
    FOREACH(month IN months | 
      CREATE (m:Month {month: month})
      MERGE (y)-[:HAS_MONTH]->(m)
      FOREACH(day IN (CASE 
                        WHEN month IN [1,3,5,7,8,10,12] THEN range(1,31) 
                        WHEN month = 2 THEN 
                          CASE
                            WHEN year % 4 <> 0 THEN range(1,28)
                            WHEN year % 100 <> 0 THEN range(1,29)
                            WHEN year % 400 <> 0 THEN range(1,29)
                            ELSE range(1,28)
                          END
                        ELSE range(1,30)
                      END) |      
        CREATE (d:Day {day: day})
        MERGE (m)-[:HAS_DAY]->(d))))

    WITH *
    MATCH (year:Year)-[:HAS_MONTH]->(month)-[:HAS_DAY]->(day)
    WITH year,month,day
    ORDER BY year.year, month.month, day.day
    WITH collect(day) as days
    FOREACH(i in RANGE(0, length(days)-2) | 
      FOREACH(day1 in [days[i]] | 
        FOREACH(day2 in [days[i+1]] | 
          CREATE UNIQUE (day1)-[:NEXT]->(day2))))" {:start 2011 :end 2014}))

(defn as-timetree [timestamp]
  (let [event-date (c/from-long timestamp)]
    { :year (t/year event-date)
      :month (t/month event-date)
      :day (t/day event-date) }))

(defn create-member [member]
  (let [social-media (:other_services member)
        query (str "MERGE (p:Person {meetupId: {person}.id})
                    SET p.name = {person}.name
                    MERGE (m:MeetupProfile {id: {person}.id})
                    SET m = {person}
                    MERGE (p)-[:HAS_MEETUP_PROFILE]->(m)
                    WITH p, m
                    MATCH (year:Year {year: {timetree}.year })
                    MATCH (year)-[:HAS_MONTH]->(month {month: {timetree}.month })
                    MATCH (month)-[:HAS_DAY]->(day {day: {timetree}.day })
                    CREATE (m)-[:JOINED_ON]->(day) 
                    FOREACH(topic IN {topics} |
                      MERGE (t:Topic {id: topic.id})
                      SET t = topic
                      MERGE (m)-[:INTERESTED_IN_TOPIC]->(t)) "
                    (if (:twitter social-media)
                      "MERGE (twitter:Twitter {id: {socialmedia}.twitter.identifier })
                       MERGE (p)-[:HAS_TWITTER_ACCOUNT]->(twitter) "
                      "")
                    (if (:linkedin social-media)
                      "MERGE (linked:LinkedIn {id: {socialmedia}.linkedin.identifier })
                       MERGE (p)-[:HAS_LINKEDIN_ACCOUNT]->(linked) "
                      "")
                    "RETURN ID(p)")
        params {:person {
                         :id (:id member)
                         :name (:name member)
                         :bio (:bio member)
                         :joined (:joined member)
                         }
                :timetree (as-timetree (:joined member))
                :socialmedia social-media
                :topics (:topics member)}]
    (tx/statement query params)))


(defn create-event [event]
  (tx/statement "MERGE (g:Group {id: {group}.id})
                 SET g = {group}
                 MERGE (e:Event {id: {event}.id})
                 SET e = {event}
                 MERGE (g)-[:HOSTED_EVENT]->(e)
                 WITH e, g
                 MATCH (year:Year {year: {timetree}.year })
                 MATCH (year)-[:HAS_MONTH]->(month {month: {timetree}.month })
                 MATCH (month)-[:HAS_DAY]->(day {day: {timetree}.day })
                 CREATE (e)-[:HAPPENED_ON]->(day) 
                 MERGE (v:Venue {id: {venue}.id})
                 SET v = {venue}
                 MERGE (e)-[:HELD_AT]->(v)
                 RETURN ID(g)"
                {:group (:group event)
                 :venue (:venue event)
                 :timetree (as-timetree (:time event))              
                 :event { :id (:id event)
                         :name (:name event)
                         :description (:description event)
                         :time (:time event)
                         :utc_offset (:utc_offset event)}}))

(defn create-rsvp [rsvp]
  (tx/statement "MATCH (e:Event {id: {event}.id})
                 MATCH (m:MeetupProfile {id: {member}.member_id})
                 FOREACH(response IN [{responses}[-1]] |
                   CREATE (rsvp:RSVP {id: {id}})
                   SET rsvp.response = response.response,
                       rsvp.guests = response.guests,
                       rsvp.time = response.time
                   MERGE (m)-[:RSVPD]->(rsvp)-[:TO]->(e)
                   MERGE (year:Year {year: response.timetree.year })
                   MERGE (year)-[:HAS_MONTH]->(month {month: response.timetree.month })
                   MERGE (month)-[:HAS_DAY]->(day {day: response.timetree.day })
                   MERGE (rsvp)-[:HAPPENED_ON]->(day))
                 FOREACH(response IN {responses}[..-1] |
                   CREATE (rsvp:RSVP {id: {id}})
                   SET rsvp.response = response.response,
                       rsvp.guests = response.guests,
                       rsvp.time = response.time
                   MERGE (m)-[:INITIALLY_RSVPD]->(rsvp)-[:TO]->(e)
                   MERGE (year:Year {year: response.timetree.year })
                   MERGE (year)-[:HAS_MONTH]->(month {month: response.timetree.month })
                   MERGE (month)-[:HAS_DAY]->(day {day: response.timetree.day })
                   MERGE (rsvp)-[:HAPPENED_ON]->(day))
                 WITH m, e
                 MATCH (m)-[:INITIALLY_RSVPD|:RSVPD]->(rsvp)-[:TO]->(e)
                 WITH rsvp
                 ORDER BY rsvp.time
                 WITH COLLECT(rsvp) AS rsvps
                 FOREACH(i in RANGE(0, length(rsvps)-2) | 
                   FOREACH(rsvp1 in [rsvps[i]] | 
                     FOREACH(rsvp2 in [rsvps[i+1]] | 
                       CREATE UNIQUE (rsvp1)-[:NEXT]->(rsvp2))))"
                {:group (:group rsvp)
                 :event (:event rsvp)
                 :member (:member rsvp)
                 :id (:rsvp_id rsvp)
                 :responses (:responses rsvp)
                 :guests (:guests rsvp)
                 }))

(defn changed-mind? [rsvp]
  (not (= (:created rsvp) (:mtime rsvp))))

(defn responses [rsvp]
  (if (changed-mind? rsvp)
    (if (= "yes" (:response rsvp))
      [{:response (:response rsvp)
        :time (:created rsvp)
        :timetree (as-timetree (:created rsvp))
        :guests (:guests rsvp)}]
      [{:response "yes"
        :time (:created rsvp)
        :timetree (as-timetree (:created rsvp))
        :guests (:guests rsvp)}
       {:response (:response rsvp)
        :time (:mtime rsvp)
        :timetree (as-timetree (:mtime rsvp))
        :guests 0}])
    [{:response (:response rsvp)
      :time (:created rsvp)
      :timetree (as-timetree (:created rsvp))      
      :guests (:guests rsvp)}]))

(defn rsvps-with-responses [rsvps]
  (map #(assoc % :responses (responses %)) rsvps))

(defn changed-mind [event-id rsvps]
  (->> rsvps
       (filter #(= (str event-id) (->> % :event :id)))
       (filter #(not (= (:created %) (:mtime %))))))

(defn load-into-neo4j []
  (create-time-tree 2011 2014)
  (db/tx-api create-member  (load "data/members-2014-04-28.json"))
  (db/tx-api create-event  (load "data/events-2014-04-28.json"))
  (db/tx-api create-rsvp (rsvps-with-responses (load "data/rsvps-2014-04-28.json"))))

(defn main []
  (save "data/members-2014-04-28.json" (get-all members))
  (save "data/events-2014-04-28.json" (get-all events))
  (save "data/rsvps-2014-04-28.json"
        (mapcat #(get-all (partial rsvps %)) (map :id (load "data/events-2014-04-28.json")))))
