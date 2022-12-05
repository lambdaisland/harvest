(ns lambdaisland.harvest.kernel-test
  (:require [lambdaisland.harvest.kernel :as hk]
            [clojure.test :refer :all]))

(def profile-factory
  {:handle #(rand-nth ["chromatic69" "headflights" "RoombaBrain"])
   :name "Jack Appleflap"
   :website "http://random.site"})

(def article-factory
  {:title "the article title"
   :profile (hk/ref ::profile)})

(def registry
  {:uuid {:harvest.factory/name :uuid
          :harvest.factory/definition random-uuid}
   ::profile {:harvest.factory/name ::profile
              :harvest.factory/definition profile-factory}
   ::article {:harvest.factory/name ::article
              :harvest.factory/definition article-factory}})

(def uuid-hooks
  {:map (fn [res qry ctx]
          (assoc-in res [:value :id] (random-uuid)))
   :ref (fn [res qry ctx]
          (if-let [id (and (map? (:value res))
                           (:id (:value res)))]
            (assoc-in res [:value (:ref qry)] id)))})

(deftest atomic-values-test
  (is (= {:value :foo :ctx {}} (hk/build {} :foo)))
  (is (= {:value 123 :ctx {}} (hk/build {} 123)))
  (is (= {:value "foo" :ctx {}} (hk/build {} "foo")))
  (is (= {:value #inst "2022-03-18" :ctx {}} (hk/build {} #inst "2022-03-18"))))

(deftest basic-registry-test
  (is (= "Arne Brasseur"
         (:value
          (hk/build {:registry {:name {:harvest.factory/definition #(str "Arne" " " "Brasseur")}}}
                    (hk/ref :name))))))

(deftest map-test
  (is (= {:name "Arne Brasseur" :age 39}
         (:value (hk/build {:registry {::name {:harvest.factory/definition #(str "Arne" " " "Brasseur")}}}
                           {:name (hk/ref ::name)
                            :age 39})))))

(hk/build {:registry registry}
          (hk/ref ::article))
