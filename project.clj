(defproject neo4j-meetup "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clj-http "0.9.1"]
                 [org.clojure/data.json "0.2.3"]
                 [clj-time "0.6.0"]
                 [clojurewerkz/neocons "2.0.1"]
                 [lib-noir "0.8.1"]
                 [compojure "1.1.6"]
                 [ring-server "0.3.1"]
                 [selmer "0.6.5"]
                 [com.taoensso/timbre "3.1.6"]
                 [com.taoensso/tower "2.0.2"]
                 [markdown-clj "0.9.41"]
                 [cheshire "5.1.2"]
                 [hiccup "1.0.2"]
                 [clojure-opennlp "0.3.2"]
                 [org.clojure/data.csv "0.1.2"]
                 [environ "0.4.0"]
                 [ring-middleware-format "0.3.2"]]
  :plugins [[lein-ring "0.8.10"]
            [lein-environ "0.4.0"]]
  :ring {:handler neo4j-meetup.handler/app
         :init    neo4j-meetup.handler/init
         :destroy neo4j-meetup.handler/destroy}

  :repl-options {:init-ns neo4j-meetup.repl}

  :jvm-opts ["-Xmx8g" "-server"] 

  :profiles
  {:uberjar {:aot :all}
   :production {:ring {:open-browser? false
                       :stacktraces?  false
                       :auto-reload?  false}}
   :dev {:dependencies [[ring-mock "0.1.5"]
                        [ring/ring-devel "1.2.2"]]
         :env {:dev true}}}
  :min-lein-version "2.0.0")
