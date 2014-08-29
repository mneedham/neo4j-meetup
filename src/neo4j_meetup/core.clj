(ns neo4j-meetup.core
  (:require [clj-http.client :as client])
  (:require [clojure.data.json :as json])
  (:require [environ.core :as e])
  (:require [clj-time.core :as t])
  (:require [clj-time.coerce :as c])
  (:require [clj-time.format :as f])
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

(defn members-of-other-group
  [{perpage :perpage offset :offset orderby :orderby group-id :groupid}]
  (->> (client/get
        (str "https://api.meetup.com/2/members?page=" perpage
             "&offset=" offset
             "&orderby=" orderby
             "&group_id=" group-id
             "&key=" MEETUP_KEY)
        {:as :json})
       :body :results))

(defn events
  [{perpage :perpage offset :offset orderby :orderby}]
  (->> (client/get
        (str "https://api.meetup.com/2/events?page=" perpage
             "&offset=" offset
             "&orderby=" orderby
             "&fields=announced_at"
             "&status=upcoming,past&"
             "&group_urlname=" MEETUP_NAME
             "&key=" MEETUP_KEY)
        {:as :json})
       :body :results))

(defn groups
  [{perpage :perpage offset :offset orderby :orderby}]
  (->> (client/get
        (str "https://api.meetup.com/2/groups?page=" perpage
             "&offset=" offset
             "&orderby=" orderby
             "&topic=nosql"
             "&lat=51.5072"
             "&lon=0.1275"
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

(defn get-all
  ([api-fn] (get-all api-fn {}))
  ([api-fn args]
                  (flatten
                   (take-while seq
                               (map #(api-fn (merge {:perpage 200 :offset % :orderby "name"} args)) (offsets))))))

(defn all-events []
  (get-all events))

(defn all-members []
  (get-all members))

(defn save [file data]
  (clojure.core/spit file (json/write-str data)))

(comment (def query-keys [:orderedPastEvents :orderedFutureEvents]))
(comment (def query-result (->> ( db/cypher sorted-query) first)))

(comment (->> query-keys
              (mapcat (fn [key]
                        [key (map #(% :data) (get query-result key))]))
              (apply  array-map)))

(defn load-json [file]
  (json/read-str (slurp file) :key-fn keyword))

(defn timed [fn description]
  (println (str description ":" (with-out-str (time (fn))))))

(def format-as-year-month-day (f/formatter "yyyy-MM-dd"))

(defn save-other-groups [date]
  (.mkdir (java.io.File. (str "data/members-" date)))
  (doseq [id (map :id (load-json (str "data/groups-" date ".json")))]
    (timed #(save (str "data/members-" date "/" id ".json")
                  (get-all members-of-other-group {:groupid id})) (str "group " id))))

(defn -main [& args]
  (let [date (f/unparse format-as-year-month-day (t/now))]
    (timed #(save (str "data/groups-" date ".json") (get-all groups)) "groups")
    (save-other-groups date)
    (timed #(save (str "data/events-" date ".json") (get-all events)) "events")
    (timed #(save (str "data/rsvps-" date ".json")
                  (mapcat (fn [data] ( get-all (partial rsvps data)))
                         (map :id (load-json (str "data/events-" date ".json"))))) "rsvps") ))
