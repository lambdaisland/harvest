(ns repl-sessions.poke-xtdb
  (:require [xtdb.api :as xt]
            [lambdaisland.harvest :as h]
            [lambdaisland.faker :refer [fake]]
            [lambdaisland.harvest.kernel :as hk]))

(def node (xt/start-node {}))

(def tx
  (xt/submit-tx node [[::xt/put
                       {:xt/id :dbpedia.resource/Pablo-Picasso
                        :first-name :Pablo}
                       ]] ))

(xt/with-tx )

(xt/await-tx node tx)

(xt/entity
 (xt/db node)
 :dbpedia.resource/Pablo-Picasso)

(defn date-of-birth []
  (let [over18-epoch-days (.toEpochDay
                           (.minusYears
                            (java.time.LocalDate/now)
                            18))]
    (java.time.LocalDate/ofEpochDay (- over18-epoch-days (rand-int 5000)))))

;; We'll have institutions (colleges or universities):

(h/defactory institution
  {:institution/name #(fake [:educator :university])})

;; And students:

(h/defactory student
  {:student/name #(fake [:name :name])
   :student/date-of-birth date-of-birth})

;; Courses have a course name, a start and end time, and are taught at a
;; specific institution. Note how we simply reference the institution factory
;; here.

(h/defactory course
  {:course/name #(fake [:educator :course-name])
   :course/start-time #(fake [:time :date-time])
   :course/end-time #(fake [:time :date-time])
   :course/institution institution})

;; Finally we allow students to enroll in courses.

(h/defactory enrollment
  {:enrollment/student student
   :enrollment/course course})


(let [{:harvest.result/keys [value linked]}
      (hk/build nil enrollment {:after-build-factory
                                (fn [ctx]
                                  (update ctx :harvest.result/value
                                          (fn [v]
                                            (update-vals
                                             (assoc v :xt/id (random-uuid))
                                             (fn [v]
                                               (if-let [id (:xt/id v)]
                                                 id
                                                 v))))))})]
  (xt/submit-tx node (into [[::xt/put value]]
                           (map #(do [::xt/put %]))
                           (vals linked))))


(xt/q (xt/db node)
      '[:find (pull ?e [{:enrollment/course [*]}
                        {:enrollment/student [*]}])
        :where [?e :enrollment/course]])

(xt/pull (xt/db node) '[*] #uuid "17d477f5-4067-4b41-8694-bb34711a9c35")
