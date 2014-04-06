(ns neo4j-meetup.core
  (:require [clj-http.client :as client])
  (:require [clojure.data.json :as json])
  (:require [environ.core :as e])
  (:require [clojurewerkz.neocons.rest :as nr]
            [clojurewerkz.neocons.rest.transaction :as tx]))

(def MEETUP_KEY (e/env :meetup-key))
(def MEETUP_NAME "graphdb-london")
(def NEO4J_HOST "http://localhost:7474/db/data/")

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

(defn tx-api [import-fn coll]
  (nr/connect! NEO4J_HOST)
  (let [transaction (tx/begin-tx)]
    (tx/with-transaction
      transaction
      true
      (let [[_ result]
            (tx/execute transaction (map import-fn coll))]
        (println result)))))

(defn tx-api-single [query]
  (nr/connect! NEO4J_HOST)
  (let [transaction (tx/begin-tx)]
    (tx/with-transaction
      transaction
      true
      (let [[_ result]
            (tx/execute transaction
                        [(tx/statement query)])]
        (first result)))))

(defn link-credo-venues [query]
  (tx-api-single "MATCH (v1:Venue {id: 9695352})
                  MATCH (v2:Venue {id: 10185422})
                  MERGE (v1)-[:ALIAS_OF]->(v2)"))


(comment (map :row
              (:data (tx-api-single "MATCH (n:MeetupProfile)
                            RETURN n.name
                            LIMIT 10"))))


(defn create-member [member]
  (let [social-media (:other_services member)
        query (str "MERGE (p:Person {meetupId: {person}.id})
                    SET p.name = {person}.name
                    MERGE (m:MeetupProfile {id: {person}.id})
                    SET m = {person}
                    MERGE (p)-[:HAS_MEETUP_PROFILE]->(m)
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
                    "RETURN ID(p)")]
    (tx/statement query
                  {:person {
                            :id (:id member)
                            :name (:name member)
                            :bio (:bio member)
                            }
                   :socialmedia social-media
                   :topics (:topics member)})))


(defn create-event [event]
  (tx/statement "MERGE (g:Group {id: {group}.id})
                 SET g = {group}
                 MERGE (e:Event {id: {event}.id})
                 SET e = {event}
                 MERGE (g)-[:HOSTED_EVENT]->(e)
                 MERGE (v:Venue {id: {venue}.id})
                 SET v = {venue}
                 MERGE (e)-[:HELD_AT]->(v)
                 RETURN ID(g)"
                {:group (:group event)
                 :venue (:venue event)
                 :event { :id (:id event)
                         :name (:name event)
                         :description (:description event)
                         :time (:time event)}}))

(defn create-rsvp [rsvp]
  (tx/statement "MATCH (e:Event {id: {event}.id})
                 MATCH (m:MeetupProfile {id: {member}.member_id})
                 MERGE (rsvp:RSVP {id: {id}})
                 SET rsvp.response = {response}, rsvp.guests = {guests}
                 MERGE (m)-[:RSVPD]->(rsvp)-[:TO]->(e)
                 RETURN ID(rsvp)"
                {:group (:group rsvp)
                 :event (:event rsvp)
                 :member (:member rsvp)
                 :id (:rsvp_id rsvp)
                 :response (:response rsvp)
                 :guests (:guests rsvp)
                 }))

(defn connect-linkedin []
  (let [meetups
        (mapcat :row (:data (tx-api-single "MATCH (n:MeetupProfile) RETURN n.name")))
        profiles
        (map #(str (:firstName %) " " (:lastName %)) (load "data/connections.json"))
        members-with-profile
        (clojure.set/intersection (set profiles) (set meetups))]
    members-with-profile))

(defn load-into-neo4j []
  (tx-api create-member  (load "data/members.json"))
  (tx-api create-event  (load "data/events.json"))
  (tx-api create-rsvp (load "data/rsvps.json")))

(defn main []
  (save "data/members.json" (get-all members))
  (save "data/events.json" (get-all events))
  (save "data/rsvps.json" (mapcat #(get-all (partial rsvps %))
                                  (map :id (load "data/events.json")))))
