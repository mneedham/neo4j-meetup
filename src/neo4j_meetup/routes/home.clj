(ns neo4j-meetup.routes.home
  (:use compojure.core)
  (:use selmer.filters)
  (:require [clj-time.format :as f])
  (:require [clj-time.coerce :as c])
  (:require [neo4j-meetup.views.layout :as layout]
            [neo4j-meetup.util :as util]
            [neo4j-meetup.core :as core]
            [neo4j-meetup.meetup :as meetup]))

(defn home-page []
  (layout/render
    "home.html" {:events (meetup/all-events core/MEETUP_NAME) }))

(add-filter! :timestamp-to-date #(f/unparse (f/formatter "dd MMMM yyyy") (c/from-long %)))
(add-filter! :guestify #(if (= % 0) "-" %))

(defn events-page [event-id]
  (let [result (meetup/event event-id) ]
   (layout/render
    "events.html" {:result result})))

(defn about-page []
  (layout/render "about.html"))

(defroutes home-routes
  (GET "/" [] (home-page))
  (GET "/events/:id" [id] (events-page id))
  (GET "/about" [] (about-page)))
