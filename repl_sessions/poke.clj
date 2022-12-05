(ns repl-sessions.poke
  (:require [lambdaisland.harvest :as h]
            [lambdaisland.harvest.kernel :as hk]
            [clojure.string :as str]))

(def short-words
  ["bat" "bar" "cat" "dud" "lip" "map" "pap" "sip" "fig" "wip"])

(defn rand-id []
  (str/join "-"
            (take 3
                  (shuffle (concat (map str/upper-case short-words)
                                   short-words)))))

(h/defactory cycle
  {:type :cycle
   :id rand-id})

(h/defactory user
  {:type :user
   :id rand-id
   :name "Finn"})

(h/defactory organization
  {:type :organization
   :id rand-id})

(h/defactory organization-user
  {:type :organization-user
   :id rand-id
   :organization-id organization
   :user-id user})

(h/defactory property
  {:type :property
   :id rand-id
   :org-id organization
   :created-by user})

(h/defactory property-cycle-user
  {:type :property-cycle-user
   :id rand-id
   :cycle-id cycle
   :property-id property
   :user-id user})

(defrecord LVar [identity])

(property-cycle-user
 {:rules {[:created-by] (->LVar :user)
          [:user-id] (->LVar :user)
          [:org-id] (->LVar :org)
          [:organization-id] (->LVar :org)}})

(h/sel
 (h/build property-cycle-user)
 [:created-by])

(h/sel
 (h/build property-cycle-user)
 [#{:user-id :created-by}])

(h/sel
 (h/build property-cycle-user)
 [user])
(keys
 (:harvest.result/linked
  (h/build property-cycle-user)))

(h/build property-cycle-user
         {:rules {[user :name] cycle
                  [property] user}})

(some #(when (hk/path-match? '[repl-sessions.poke/property-cycle-user :user-id repl-sessions.poke/user :name] (key %)) (val %)) {[user :name] "Jake"
                                                                                                                                 [property] {:foo "bar"}})


(def kk2
  (keys
   (:harvest.result/linked
    (h/build property-cycle-user
             #_{:rules {[user] (hk/->LVar :x)}}))))

(count kk2)

(remove (set kk) kk2)

(h/build-val [property-cycle-user
              organization-user]
             {:rules {[#{:org-id :organization-id}] (hk/->LVar :x)}})


(h/build-val [property-cycle-user
              organization-user]
             {:rules {[organization] (hk/->LVar :x)}})


(hk/path-match? `[0 repl-sessions.poke/property-cycle-user :property-id repl-sessions.poke/property :org-id repl-sessions.poke/organization]
                [#{:org-id :organization-id}])

(h/defactory a
  {:a #(rand-int 100)})

(h/defactory b
  {:a1 a
   :a2 a
   :b "b"})

(h/defactory c
  {:b1 b
   :b2 b
   :c "c"})

(keys (:harvest.result/linked (h/build c {:rules {a (h/unify)}})))
([repl-sessions.poke/c :b1 repl-sessions.poke/b :a1 repl-sessions.poke/a]
 [repl-sessions.poke/c :b1 repl-sessions.poke/b :a2 repl-sessions.poke/a]
 [repl-sessions.poke/c :b1 repl-sessions.poke/b]
 [repl-sessions.poke/c :b2 repl-sessions.poke/b :a1 repl-sessions.poke/a]
 [repl-sessions.poke/c :b2 repl-sessions.poke/b :a2 repl-sessions.poke/a]
 [repl-sessions.poke/c :b2 repl-sessions.poke/b]
 [repl-sessions.poke/c])
