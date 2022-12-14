(ns lambdaisland.harvest.datomic-peer-test
  (:require [clojure.test :refer :all]
            [datomic.api :as d]
            [lambdaisland.harvest :as h]
            [lambdaisland.harvest.datomic-peer :as fd]
            [lambdaisland.harvest.kernel :as hk]))

(defn s [sname type & {:as opts}]
  (merge
   {:db/ident sname
    :db/valueType (keyword "db.type" (name type))
    :db/cardinality :db.cardinality/one}
   opts))

(h/defactory line-item
  {:line-item/description "Widgets"
   :line-item/quantity 5
   :line-item/price 1.0})

(h/defactory cart
  {:cart/created-at #(java.util.Date.)
   :cart/line-items [line-item line-item]})

(def schema
  [{:db/ident       :line-item/description,
    :db/valueType   :db.type/string,
    :db/cardinality :db.cardinality/one}
   {:db/ident       :line-item/quantity,
    :db/valueType   :db.type/long,
    :db/cardinality :db.cardinality/one}
   {:db/ident       :line-item/price,
    :db/valueType   :db.type/double,
    :db/cardinality :db.cardinality/one}
   {:db/ident       :cart/created-at,
    :db/valueType   :db.type/instant,
    :db/cardinality :db.cardinality/one}
   {:db/ident       :cart/line-items,
    :db/valueType   :db.type/ref,
    :db/cardinality :db.cardinality/many}])

(deftest basic-datomic-test
  (let [url (doto (str "datomic:mem://db" (rand-int 1e8))
              d/create-database)
        conn (d/connect url)]
    @(d/transact conn schema)
    (let [{:harvest.result/keys [linked value]
           ::fd/keys [db-after]
           :as res} (fd/create! conn cart)]
      (is (= #{:cart/created-at :cart/line-items :db/id}
             (set (keys value))))
      (is (= "Widgets"
             (get-in linked [[`cart :cart/line-items 0 `line-item]
                             :line-item/description])))
      (is (= "Widgets" (get (fd/entity res [:cart/line-items 0]) :line-item/description))))))
