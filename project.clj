(defproject neo4j-meetup "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clj-http "0.9.1"]
                 [org.clojure/data.json "0.2.3"]
                 [clj-time "0.6.0"]
                 [clojurewerkz/neocons "2.0.1"]
                 [compojure "1.1.5"]
                 [cheshire "5.1.2"]
                 [hiccup "1.0.2"]
                 [clojure-opennlp "0.3.2"]
                 [environ "0.4.0"]]
  :plugins [[lein-ring "0.8.3"]]
  :ring {:handler neo4j-meetup.linkedin/app})
