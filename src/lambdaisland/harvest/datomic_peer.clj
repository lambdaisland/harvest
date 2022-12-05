(ns lambdaisland.harvest.datomic-peer
  (:require [clojure.walk :as walk]
            [datomic.api :as d]
            [lambdaisland.harvest :as h]
            [lambdaisland.harvest.kernel :as hk]))

(defn update-tempids [coll tempids]
  (walk/postwalk (fn [o]
                   (if-let [tid (and (string? o) (get tempids o))]
                     tid
                     o))
                 coll))

(defn walk-entity->id [coll]
  (walk/postwalk (fn [e]
                   (if-let [id (:db/id e)]
                     id
                     e))
                 coll))

(defn transact-result! [conn result]
  (let [{:keys [tempids db-after]} @(d/transact conn [(:harvest.result/value result)])]
    (-> result
        (update :harvest.result/value update-vals walk-entity->id)
        (update :harvest.result/linked update-vals (fn [v] (update-vals v walk-entity->id)))
        (update :harvest.result/value update-tempids tempids)
        (update :harvest.result/linked update-tempids tempids)
        (assoc ::db-after db-after))))

(defn create!
  "Create datomic entities based on the factory and any factories it links to."
  ([conn factory]
   (create! conn factory nil))
  ([conn factory opts]
   (as-> opts $
     (assoc $ :after-build-factory
            (fn [ctx]
              (update ctx :harvest.result/value
                      assoc :db/id (str (gensym "harvest.datomic/tempid")))))
     (hk/build nil factory $)
     (transact-result! conn $))))

(defn entity [result selector]
  (when-let [id (:db/id (h/sel1 result selector))]
    (d/entity (::db-after result) id)))
