(ns repl-sessions.poke
  (:require [lambdaisland.facai :as f]
            [lambdaisland.facai.kernel :as fk]
            [clojure.string :as str]))

(def short-words
  ["bat" "bar" "cat" "dud" "lip" "map" "pap" "sip" "fig" "wip"])

(defn rand-id []
  (str/join "-"
            (take 3
                  (shuffle (concat (map str/upper-case short-words)
                                   short-words)))))

(f/defactory cycle
  {:type :cycle
   :id rand-id})

(f/defactory user
  {:type :user
   :id rand-id
   :name "Finn"})

(f/defactory organization
  {:type :organization
   :id rand-id})

(f/defactory organization-user
  {:type :organization-user
   :id rand-id
   :organization-id organization
   :user-id user})

(f/defactory property
  {:type :property
   :id rand-id
   :org-id organization
   :created-by user})

(f/defactory property-cycle-user
  {:type :property-cycle-user
   :id rand-id
   :cycle-id cycle
   :property-id property
   :user-id user})

(f/build-val property-cycle-user)

;; This is a factory
property-cycle-user

;; This is a "thunk", but conceptually it's also a factory
(property-cycle-user :with {:id "123"})

;; both of these can be built
(f/build-val property-cycle-user)

;; This is a "thunk", but conceptually it's also a factory
(f/build-val (property-cycle-user :with {:id "123"}))

;; But the options can also be passed to build-val... or rather, the former
;; syntax is a way of bundling specific "build-val" options with the factory
(f/build-val property-cycle-user :with {:id "123"})

;; Besides building a single value (the simplest case), we can build a
;; resultset, which contains info about all the linked objects, and which can be
;; queried with selectors.
(f/build property-cycle-user)
(let [res (f/build property-cycle-user)
      cycle (f/sel1 res cycle)
      user (f/sel1 res user)
      property (f/sel1 res property)]
  [cycle user property])

;; This used to simply return a value, but now it doesn't, the reason is that we
;; want to be able to use these within factory definitions, or when specifying
;; overrides
(property-cycle-user :with {:id "123"})

(f/build-val
 property-cycle-user
 :with {:id "123"
        :user (user :with {:name "Timmy"})})

(-> property-cycle-user
    (f/with :user (f/with user :name "Timmy"))
    (f/rule [user :name] "Timmy")
    (f/sel [user [:* property] cycle])
    deref)
