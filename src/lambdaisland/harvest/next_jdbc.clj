(ns lambdaisland.harvest.next-jdbc
  (:require [clojure.string :as str]
            [inflections.core :as inflections]
            [camel-snake-kebab.core :as csk]
            [lambdaisland.harvest.kernel :as hk]
            [camel-snake-kebab.core :as csk]
            [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [next.jdbc.quoted :as quoted]
            [next.jdbc.result-set :as result-set]))

(defn as-kebab-maps [rs opts]
  (let [kebab #(str/replace % #"_" "-")]
    (result-set/as-modified-maps rs (assoc opts :qualifier-fn (constantly nil) :label-fn kebab))))

(defn table-fn [fact]
  (inflections/plural (name (hk/factory-id fact))))

(def default-ctx
  {::primary-key :id
   ::quote-fn quoted/ansi
   ::table-fn table-fn
   ::fk-col-fn identity
   ::insert-opts {:builder-fn as-kebab-maps
                  :column-fn (comp quoted/ansi csk/->snake_case_string)}})

(defn create!
  ([jdbc-opts factory]
   (create! jdbc-opts factory nil))
  ([jdbc-opts factory opts]
   (let [ctx (merge
              default-ctx
              (if (::ds jdbc-opts)
                jdbc-opts
                {::ds jdbc-opts}))]
     (hk/build ctx factory
               (-> opts
                   (update :association-key (fnil comp identity)
                           (fn [{::keys [fk-col-fn]} k]
                             (fk-col-fn k)))
                   (update :after-build-factory (fnil comp identity)
                           (fn [{::keys [ds primary-key table-fn insert-opts quote-fn]
                                 :harvest.result/keys [value]
                                 :harvest.build/keys [path]
                                 factory :harvest.build/input
                                 :as ctx}]
                             (let [table (quote-fn (or (::table factory)
                                                       (table-fn factory)))
                                   row (sql/insert! ds table value insert-opts)
                                   pk (or (:harvest.factoryory/primary-key factory) primary-key)
                                   value (merge (:harvest.result/value ctx) row)]
                               (-> ctx
                                   (assoc :harvest.result/value (if (< 1 (count path))
                                                                (get value pk)
                                                                value))
                                   (hk/add-linked path value))))))))))
