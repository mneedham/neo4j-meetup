(ns neo4j-meetup.import
  (:require [clojure.data.json :as json])
  (:require [environ.core :as e])
  (:require [clj-time.core :as t])
  (:require [clj-time.coerce :as c])
  (:require [neo4j-meetup.db :as db])
  (:require [clojurewerkz.neocons.rest :as nr]
            [clojurewerkz.neocons.rest.transaction :as tx]))

(defn clear-all []
  (db/tx-api-single "
    MATCH (n)
    OPTIONAL MATCH (n)-[r]-(m)
    DELETE n,r,m")
  (db/tx-api-single "CREATE INDEX ON :Person(meetupId)")
  (db/tx-api-single "CREATE INDEX ON :MeetupProfile(id)")
  (db/tx-api-single "CREATE INDEX ON :Group(id)")
  (db/tx-api-single "CREATE INDEX ON :Year(year)")
  (db/tx-api-single "CREATE INDEX ON :Month(month)")
  (db/tx-api-single "CREATE CONSTRAINT ON (t:Topic) ASSERT t.id IS UNIQUE")
  (db/tx-api-single "CREATE INDEX ON :Twitter(id)")
  (db/tx-api-single "CREATE INDEX ON :LinkedIn(id)"))

(defn as-timetree [timestamp]
  (let [event-date (c/from-long timestamp)]
    { :year (t/year event-date)
      :month (t/month event-date)
      :day (t/day event-date) }))

(defn create-member-topics [member]
  (let [query (str "MATCH (m:MeetupProfile {id: {person}.id})
                    FOREACH(topic IN {topics} |
                      MERGE (t:Topic {id: topic})
                      MERGE (m)-[:INTERESTED_IN]->(t))")
        params {:person {
                         :id (:id member)
                         }
                :topics (map :id (:topics member))
                }]
    (tx/statement query params)))

(defn create-member [member]
  (let [social-media (:other_services member)
        query (str "MERGE (p:Person {meetupId: {person}.id})
                    ON CREATE SET p.name = {person}.name
                    MERGE (m:MeetupProfile {id: {person}.id})
                    ON CREATE SET m = {person}
                    MERGE (p)-[:HAS_MEETUP_PROFILE]->(m)
                    WITH p, m
                    MATCH (g:Group {id: {groupid}})
                    MERGE (m)-[:MEMBER_OF]->(g)
                    WITH p,m
                    MATCH (year:Year {year: {timetree}.year }),
                          (year)-[:HAS_MONTH]->(month {month: {timetree}.month }),
                          (month)-[:HAS_DAY]->(day {day: {timetree}.day })
                    CREATE (m)-[:JOINED_ON]->(day) "
                    (if (:twitter social-media)
                      "MERGE (twitter:Twitter {id: {socialmedia}.twitter.identifier })
                       MERGE (p)-[:HAS_TWITTER_ACCOUNT]->(twitter) "
                      "")
                    (if (:linkedin social-media)
                       "MERGE (linked:LinkedIn {id: {socialmedia}.linkedin.identifier })
                        MERGE (p)-[:HAS_LINKEDIN_ACCOUNT]->(linked) "
                       ""))
        params {:person {
                         :id (:id member)
                         :name (:name member)
                         :bio (:bio member)
                         :joined (:joined member)
                         :photolink (->> member :photo :photo_link)
                         :thumbnail (->> member :photo :thumb_link)
                         }
                :groupid (:groupid member)
                :timetree (as-timetree (:joined member))
                :socialmedia social-media
                }]
    (tx/statement query params)))

(defn create-event-types [start-year end-year]
  (db/tx-api-single "
    MERGE (intro:EventType {name: 'Intro to Graphs'})
    MERGE (cypher:EventType {name: 'Hands on Cypher'})
    MERGE (modelling:EventType {name: 'Intro to Modelling'})
    MERGE (app:EventType {name: 'Hands on Build an App'})
    MERGE (normal:EventType {name: 'Normal Last Wednesday' })
    MERGE (graphPub:EventType {name: 'Graph Pub' }) 

    MERGE (intro)-[:NEXT]->(cypher)
    MERGE (intro)-[:NEXT]->(modelling)
    MERGE (intro)-[:NEXT]->(normal)
    MERGE (intro)-[:NEXT]->(graphPub)
    MERGE (cypher)-[:NEXT]->(app)" {}))

(defn create-event-types  []
  (db/tx-api-single "
    MATCH (e:Event {name: 'Intro to Graphs'}) 
    MATCH (type:EventType {name: 'Intro to Graphs'}) 
    MERGE (e)-[:MEETUP_TYPE]->(type)

    UNION

    MATCH (e:Event) WHERE e.name =~ '.*(Pub|Café).*'
    MATCH (graphPub:EventType {name: 'Graph Pub' }) 
    MERGE (e)-[:MEETUP_TYPE]->(graphPub)

    UNION

    MATCH (e:Event) WHERE e.name =~ '(?i).*Hands On.*' AND e.name =~ '(?i).*cypher.*'
    MATCH (cypher:EventType {name: 'Hands on Cypher'})
    MERGE (e)-[:MEETUP_TYPE]->(cypher) 

    UNION

    MATCH (e:Event) WHERE e.name =~ '(?i).*Hands On.*' AND e.name =~ '(?i).*app.*'
    MATCH (app:EventType {name: 'Hands on Build an App'})
    MERGE (e)-[:MEETUP_TYPE]->(app)     
" {}))

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

(defn create-event [event]
  (tx/statement "MATCH (g:Group {id: {group}.id})
                 MERGE (e:Event {id: {event}.id})
                 ON CREATE SET e = {event}
                 MERGE (g)-[:HOSTED_EVENT]->(e)
                 WITH e, g
                 MATCH (year:Year {year: {timetree}.year })
                 MATCH (year)-[:HAS_MONTH]->(month {month: {timetree}.month })
                 MATCH (month)-[:HAS_DAY]->(day {day: {timetree}.day })
                 CREATE (e)-[:HAPPENED_ON]->(day) 
                 MERGE (v:Venue {id: {venue}.id})
                 ON CREATE SET v = {venue}
                 MERGE (e)-[:HELD_AT]->(v)"
                {:group (:group event)
                 :venue (:venue event)
                 :timetree (as-timetree (:time event))              
                 :event { :id (:id event)
                         :name (:name event)
                         :description (:description event)
                         :time (:time event)
                         :utc_offset (:utc_offset event)}}))

(defn create-group [group]
  (tx/statement "MERGE (g:Group {id: {group}.id})
                 SET g = {group}
                 WITH g
                 UNWIND {topics} AS topic
                 MATCH (t:Topic {id: topic.id})
                 MERGE (g)-[:HAS_TOPIC]->(t)"
                {:group {:id (:id group)
                         :city (:city group)
                         :name (:name group)
                         :description (:description group)
                         :created (:created group)
                         }
                 :topics (:topics group)}))

(defn create-topic [topic]
  (tx/statement "CREATE (t:Topic {topic})" {:topic topic}))

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

(defn link-credo-venues []
  (db/tx-api-single "MATCH (v1:Venue {id: 9695352})
                  MATCH (v2:Venue {id: 10185422})
                  MERGE (v1)-[:ALIAS_OF]->(v2)"))

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

(defn load-json [file]
  (json/read-str (slurp file) :key-fn keyword))

(defn member-files [dir]
  (filter #(.isFile %) (file-seq (clojure.java.io/file dir))) )

(defn extract-group-id [file-name]
  (read-string (clojure.string/replace (.getName file-name) #".json" "")))

(defn timed [fn description]
  (println (str description ":" (with-out-str (time (fn))))))

(defn save-json [file data]
  (clojure.core/spit file (json/write-str data)))

(defn import-topics [member-files]
  (db/tx-api create-topic (->> member-files
                               (map #(load-json (.getPath %)))
                               (mapcat (fn [data] (map #(:topics %) data)))
                               flatten
                               (clojure.core/set))))

(defn -main [& args]
  (let [date (nth args 0)
        member-files (member-files (str "data/members-" date))]
    (timed clear-all "clear")
    (timed #(create-time-tree 2011 2014) "time-tree")
    (timed #(import-topics member-files) "topics")
    (timed #(db/tx-api create-group (load-json (str "data/groups-" date ".json")))
           "groups")
    (doseq [file  member-files]
      (let [coll (map (fn [data] (merge {:groupid (extract-group-id file)} data))
                      (load-json (.getPath file)))]
        (timed #(db/tx-api create-member coll)
               (str "members of " (extract-group-id file)))
        (timed #(db/tx-api create-member-topics coll)
               (str "members topics of " (extract-group-id file)))))
    (timed #(db/tx-api create-event  (load-json (str "data/events-" date ".json")))
           "events")
    (timed #(db/tx-api create-rsvp
                       (rsvps-with-responses
                        (load-json (str "data/rsvps-" date ".json"))))
           "rsvps")
    (timed #(link-credo-venues) "credo venues")))
