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


(defn -main [& args]
  (save "data/members-2014-05-09.json" (get-all members))
  (save "data/events-2014-05-09.json" (get-all events))
  (save "data/rsvps-2014-05-09.json"
        (mapcat #(get-all (partial rsvps %)) (map :id (load "data/events-2014-05-09.json")))))
