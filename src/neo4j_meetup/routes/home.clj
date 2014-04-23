(ns neo4j-meetup.routes.home
  (:use compojure.core)
  (:require [neo4j-meetup.views.layout :as layout]
            [neo4j-meetup.util :as util]))

(defn home-page []
  (layout/render
    "home.html" {:content (util/md->html "/md/docs.md")}))

(defn about-page []
  (layout/render "about.html"))

(defroutes home-routes
  (GET "/" [] (home-page))
  (GET "/about" [] (about-page)))
