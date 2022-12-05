(ns scratch.demo-factories)

(ns lambdaisland.harvest.demo-factories
  (:require [lambdaisland.harvest :as harvest]
            [lambdaisland.harvest.kernel :as hk]
            [lambdaisland.harvest.helpers :as zh]))

(harvest/defactory ::user
  {:user/name "John Doe"
   :user/handle (harvest/sequence #(str "john" %))
   :user/email (harvest/with [:user/handle]
                         (fn [handle]
                           (str handle "@doe.com")))
   :user/roles #{}}

  :traits
  {:admin
   {:user/roles #{:admin}}})

(harvest/defactory ::member
  :inherit ::user
  {:membership_expires #(zh/days-from-now 100)})

(harvest/defactory ::article
  {:author (harvest/ref :harvest/user)
   :title "7 Tip-top Things To Try"}
  :traits
  {:published {:status "published"}
   :unpublished {:status "unpublished"}
   :in-the-future {:published-at #(zh/days-from-now 2)}
   :in-the-past {:published-at #(zh/days-ago 2)}})




(comment
  (harvest/build ::user)
  (harvest/build ::user {:user/handle "timmy"})
  (harvest/build ::user {} {:traits [:admin]})
  (harvest/build ::article {} {::harvest/traits [:published :in-the-future]})

  (harvest/build ::article
             {[::article :> :title] ""
              ::harvest/traits [:published :in-the-future]
              [:author :user/handle] "timmy"
              [:author ::harvest/traits] [:admin]})

  (hk/build {:registry @harvest/registry
             :hooks [{:ref (fn [result qry ctx]
                             (prn :ref qry '-> result)
                             result)
                      :map (fn [result qry ctx]
                             (prn :map qry '-> result)
                             result)}]}
            (harvest/ref :harvest/article))

  (harvest/build {:john :harvest/user
              :mick :harvest/admin}
             {[:john :user/handle] "johny"
              [:mick :user/handle] "micky"})

  (harvest/build (vec (repeat 5 :harvest/user))
             {[:> 0 :user/handle] "foo"})

  (harvest/build-all :harvest/article
                 {}
                 {:hooks [{:map-entry (fn [result query _]
                                        (if (hk/ref? (val query))
                                          (update-in result [:value (key query)]  :id)
                                          result)
                                        )
                           :ref (fn [result _ _]
                                  (prn [:ref (:value result)])
                                  (if (map? (:value result))
                                    (update result :value assoc :id (rand-int 100))
                                    result))}]})


  )
