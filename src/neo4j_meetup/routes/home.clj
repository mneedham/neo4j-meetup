(ns neo4j-meetup.routes.home
  (:use compojure.core)
  (:use selmer.filters)
  (:require [clj-time.format :as f])
  (:require [clj-time.coerce :as c])
  (:require [neo4j-meetup.views.layout :as layout]
            [neo4j-meetup.util :as util]
            [neo4j-meetup.core :as core]
            [neo4j-meetup.meetup :as meetup]
            [neo4j-meetup.timestamp :as timestamp]))

(defn time-descending [row]
  (* -1 (->> row :event :data :time)))

(defn time-ascending [row]
  (->> row :event :data :time))

(defn home-page []
  (let [now
        (c/to-long (clj-time.core/now))
        all
        (meetup/all-events core/MEETUP_NAME)
        {upcoming true past false }
        (group-by #(> (->> % :event :data :time) now) all)]
    (layout/render
     "home.html" {:past (sort-by time-descending past)
                  :upcoming (sort-by time-ascending upcoming)})))

(add-filter! :timestamp-to-date #(if (= % nil) "-" (timestamp/as-date %)))
(add-filter! :guestify #(if (= % 0) "-" %))

(defn choose-tag-size [freq]
  (cond (> freq 40) 10
        (> freq 30) 9
        (> freq 20) 8
        (> freq 10) 7
        (> freq 5) 5
        :else 4))
(add-filter! :tag-size choose-tag-size)

(defn events-page [event-id]
  (let [result (meetup/event event-id) ]
   (layout/render
    "events.html" {:result result})))

(defn members-page
  ([]
     (let [result (meetup/all-members)]
       (layout/render
        "members.html" {:result result})))
  ([member-id]
     (layout/render
      "member.html" {})))

(defn groups-page
  ([]
     (let [result (meetup/all-groups)]
        (layout/render
         "groups.html" {:result result})))
  ([group-id]
     (let [result (meetup/group group-id)
           topics (meetup/group-topics group-id)
           other-groups (meetup/other-groups group-id)]
       (layout/render
     "group.html" {:result result :topics topics :otherGroups other-groups}))))

(defn topics-page
  ([]
     (let [result (meetup/all-topics)]
        (layout/render
         "topics.html" {:result result})))
  ([topic-id]
     (let [result (meetup/topic topic-id)
           topics (meetup/topic-overlap topic-id)
           ]
       (layout/render
     "topic.html" {:result result :topics topics}))))

(defn venues-page [venue-id]
  (let [result (meetup/venue venue-id) ]
   (layout/render
    "venue.html" {:result result})))

(defn about-page []
  (layout/render "about.html"))

(defn as-rows-cols [members]
  {:rows (map (fn [row] {:name (->> row :profile :data :name)
                        :rsvp-yes (->> row :rsvps)
                        :join-date (->> row :profile :data :joined)
                        :most-recent (->> row :recent :event :data :name)
                        :event-date (->> row :recent :event :data :time)}) members)
   :cols [ { :type "str" :key "name" :label "Name"}
           { :type "timestamp" :key "join-date" :label "Join Date" }
           { :type "int" :key "rsvp-yes" :label "RSVP'd yes"}           
           { :type "str" :key "most-recent" :label "Most Recent Event"}
           { :type "timestamp" :key "event-date" :label "Event Date"}] 
   }
  )

(defroutes home-routes
  (GET "/" [] (home-page))
  (GET "/events/:id" [id] (events-page id))
  (GET "/members" [] (members-page))
  (GET "/members/2" [] {:body (as-rows-cols (meetup/all-members))})
  (GET "/groups" [] (groups-page))
  (GET "/groups/overlap" []  {:body  (meetup/group-overlap)})
  (GET "/groups/overlap/venn" request
       {:body (meetup/group-overlap
               (clojure.string/split (get (->> request :query-params) "ids") #","))})
  (GET "/groups/:id" [id] (groups-page id))
  (GET "/topics/:id" [id] (topics-page id))
  (GET "/topics" [] (topics-page))      
  (GET "/members/:id" [id] (members-page id))
  (GET "/venues/:id" [id] (venues-page id))
  (GET "/about" [] (about-page)))
