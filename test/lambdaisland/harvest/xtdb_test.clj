(ns lambdaisland.harvest.xtdb-test
  (:require [clojure.test :refer :all]
            [lambdaisland.harvest :as h]
            [lambdaisland.harvest.kernel :as hk]
            [lambdaisland.harvest.xtdb :as fxt]
            [xtdb.api :as xt]))

(h/defactory institution
  {:institution/name "Ghent University"})

(h/defactory course
  {:course/name "Formele Logica I"
   :course/institution institution})

(deftest create-test
  (let [node (xt/start-node {})
        result (fxt/create!
                node course
                {:xt-id-fn
                 (let [i (atom 0)]
                   (fn [_]
                     (keyword (str "entity" (swap! i inc)))))})]
    (is (= {:institution/name "Ghent University"
            :xt/id :entity1}
           (xt/pull (xt/db node) '[*] :entity1)))

    (is (= {:course/name "Formele Logica I"
            :course/institution :entity1
            :xt/id :entity2}
           (xt/pull (xt/db node) '[*] :entity2)))

    (is (= {:course/name "Formele Logica I"
            :course/institution :entity1
            :xt/id :entity2}
           (:harvest.result/value result)))

    (is (= '{[lambdaisland.harvest.xtdb-test/course
              :course/institution
              lambdaisland.harvest.xtdb-test/institution]
             {:institution/name "Ghent University"
              :xt/id :entity1}

             [lambdaisland.harvest.xtdb-test/course]
             {:course/name "Formele Logica I"
              :course/institution :entity1
              :xt/id :entity2}}
           (:harvest.result/linked result)))))
