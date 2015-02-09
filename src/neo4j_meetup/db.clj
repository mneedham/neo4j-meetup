(ns neo4j-meetup.db
  (:require [clj-http.client :as client])
  (:require [clojure.data.json :as json])
  (:require [environ.core :as e])
  (:require [clj-time.core :as t])
  (:require [clj-time.coerce :as c])
  (:require [clojure.walk :as walk])
  (:require [clojurewerkz.neocons.rest.cypher :as cy])
  (:require [clojurewerkz.neocons.rest :as nr]
            [clojurewerkz.neocons.rest.transaction :as tx]))

(def NEO4J_HOST "http://localhost:7474/db/data/")
(comment (def NEO4J_HOST "http://localhost:7521/db/data/"))

(defn tx-api [import-fn coll]
  (nr/connect! NEO4J_HOST)
  (let [transaction (tx/begin-tx)]
    (tx/with-transaction
      transaction
      true
      (let [items (map import-fn coll)
            [_ result] (time (tx/execute transaction items))]
        ))))

(defn tx-api-single
  ([query] (tx-api-single query {}))
  ([query params]
      (nr/connect! NEO4J_HOST)
      (let [transaction (tx/begin-tx)]
        (tx/with-transaction
          transaction
          true
          (let [[_ result]
                (tx/execute transaction [(tx/statement query params)])]
            (first result))))))

(defn cypher
  ([query] (cypher query {}))
  ([query params]
     (let [conn (nr/connect! NEO4J_HOST)]
       (->> (cy/tquery query params)
            walk/keywordize-keys))))
