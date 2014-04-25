(ns neo4j-meetup.routes.home
  (:use compojure.core)
  (:require [neo4j-meetup.views.layout :as layout]
            [neo4j-meetup.util :as util]
            [neo4j-meetup.core :as core]
            [neo4j-meetup.meetup :as meetup]))

(defn home-page []
  (layout/render
    "home.html" {:events (meetup/all-events core/MEETUP_NAME) }))

(defn about-page []
  (layout/render "about.html"))

(defroutes home-routes
  (GET "/" [] (home-page))
  (GET "/about" [] (about-page)))
