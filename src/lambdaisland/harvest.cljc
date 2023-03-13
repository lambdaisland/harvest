(ns lambdaisland.harvest
  (:require [clojure.walk :as walk]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining factories

(defn- qualify-sym
  [env s]
  (if (:ns env)
    ;; cljs
    (symbol (name (-> env :ns :name)) (name s))

    ;; clj
    (symbol (str *ns*) (str s))))

(defmacro <<- [& forms]
  (cons '->> (reverse forms)))

(defn factory? [o]
  (and (map? o)
       (contains? o :harvest.factory/id)))

(defn factory-id [o]
  (when (factory? o)
    (:harvest.factory/id o)))

(defn invoke-factory [f & args]
  (assoc f :harvest.factory/argv (vec args)))

(defn with [f & {:as kvs}]
  (assoc f :harvest.factory/with kvs))

(defn rules [f & rules]
  (assoc f :harvest.factory/rules rules))

(defn select [f selection]
  (assoc f :harvest.factory/selection selection))

;; RULE           := rule | {kw rule ...}
;; rule           := simple-rule | qualified-rule
;; qualified-rule := [`:*` simple-rule] | [`:1` simple-rule]
;; simple-rule    := segment | [segment ...]
;; segment        := factory-id | map-kw | `:>`


;; ;; Things you can select
;; ;; A single factory type
;; (select [:1 user])
;; ;; A map key
;; (select [:* :author])

;; ;; A map with the values selections
;; (select {:user user})
;; (select {:user [:1! user 1]})

;; ;; A vector with the values selections?
;; (select [[:submitter]
;;          [article]])

;; ;; Not allowed, only accept one arg, return type matches arg type
;; (select [:submitter]
;;         [article])

(defrecord Factory []
  #?@(:clj
      (clojure.lang.IFn
       (invoke [this] this)
       (invoke [this a] (invoke-factory this a))
       (invoke [this a b] (invoke-factory this a b))
       (invoke [this a b c] (invoke-factory this a b c))
       (invoke [this a b c d] (invoke-factory this a b c d))
       (invoke [this a b c d e] (invoke-factory this a b c d e))
       (applyTo [this args] (apply invoke-factory args)))
      :cljs
      (cljs.core/IFn
       (-invoke [this] #_(build-val this nil))
       (-invoke [this opts] #_(build-val this opts)))))

(defmacro defactory [name argv & body]
  `(def ~name
     ~(loop [fact {:harvest.factory/id `'~(qualify-sym &env name)
                   :harvest.factory/entity? true
                   :harvest.factory/state (atom {})}
             [x & xs] body]
        (cond
          (nil? x)
          `(map->Factory ~fact)
          (simple-keyword? x)
          (recur (assoc fact (keyword "harvest.factory" (clojure.core/name x)) (first xs))
                 (next xs))
          (qualified-keyword? x)
          (recur (assoc fact x (first xs))
                 (next xs))
          :else
          (recur (assoc fact :harvest.factory/template `(fn ~(if (seq argv)
                                                               (into '[this &] (remove '#{&}) argv)
                                                               '[this & _]) ~x))
                 xs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Build process

;; Stack based — recursive calls push/pop (with binding), propagates down but
;; not up
(def ^:dynamic *context* nil)

;; Bound to a mutable container during a single build pass, individual factories
;; side effect to add in their buildset.
(def ^:dynamic *buildset* nil)

(defn match1? [p s]
  (or (= s p)
      (= :* s)
      (and (set? s) (some (partial match1? p) s))
      (when-let [id (factory-id s)]
        (match1? p id))))

(defn path-match? [path selector]
  (let [selector (if (sequential? selector) selector [selector])
        path (if (= ::root (first path))
               (next path)
               path)
        path (if (and (not (or (symbol? (last selector))
                               (factory? (last selector))
                               (= :* (last selector))))
                      (symbol? (last path)))
               (butlast path)
               path)]
    (when (seq path)
      (loop [[p & ps] path
             [s & ss] selector
             i        0]
        (when (< 10 i) (throw (Exception. "too much recursion")))
        (cond
          (and (nil? p) (nil? s))
          true

          (or (nil? p) (nil? s))
          false

          (match1? p s)
          (do
            (if (seq ps)
              (or (when (seq ss)
                    (path-match? ps ss))
                  (path-match? ps (cons s ss)))
              (and (empty? ps) (empty? ss))))

          (= s :>)
          (if (match1? p (first ss))
            (recur ps (next ss) (inc i))
            false)

          :else
          (recur ps (cons s ss) (inc i)))))))

(defmacro with-path-segments [segments & body]
  `(binding [*context* (update *context* :path (fnil into []) ~segments)]
     ~@body))

(declare build)

(defn build-template* [tmpl]
  (cond
    (map? tmpl)
    (into (empty tmpl)
          (map (fn [[k v]]
                 (with-path-segments [k]
                   [k (if (factory? v)
                        (build v)
                        v)])))
          tmpl)

    (coll? tmpl)
    (let [s (map-indexed (fn [idx v]
                           (with-path-segments [idx]
                             (build v)))
                         tmpl)]
      (if (seq? tmpl)
        s
        (into (empty tmpl) s)))

    :else
    tmpl))

(defn build-template [tmpl]
  (let [{:keys [path]} *context*
        value (build-template* tmpl)]
    (vswap! *buildset* assoc path value)
    value))

(defn build-factory [{:harvest.factory/keys [id template argv rules with state] :as factory}]
  (with-path-segments [id]
    (build-template (apply template state argv))))

(defn build* [input]
  (binding [*context* {:path (:path *context* [::root])
                       :rules (into (:rules *context* []) (:harvest.factory/rules input))}
            *buildset* (or *buildset* (volatile! {}))]
    (let [value (if (factory? input)
                  (build-factory  input)
                  (build-template input))]
      [value @*buildset*])))

(defn select* [buildset selector]
  (keep (fn [[path value]]
          (when (path-match? path selector)
            value))
        buildset))

(defn select-n [buildset selector]
  (cond
    (and (vector? selector)
         (= :1 (first selector)))
    (first (select* buildset (second selector)))

    (and (vector? selector)
         (= :* (first selector)))
    (select* buildset (second selector))

    :else
    (select* buildset selector)))

(defn handle-selection [buildset selector]
  (cond
    (factory? selector)
    (select-n buildset [selector])

    (map? selector)
    (update-vals selector (partial select-n buildset))

    (and (vector? selector)
         (vector? (first selector)))
    (mapv (partial select-n buildset) selector)

    :else
    (select-n buildset selector)))

(defn build [factory]
  (let [[value buildset] (build* factory)]
    (if-let [selection (:harvest.factory/selection factory)]
      (handle-selection buildset selection)
      value)))

(defn buildset [factory]
  (second (build* factory)))



;;;;
(defactory user []
  (let [i (:cnt (swap! this update :cnt (fnil inc 0)))]
    {:user/name   "Lilliam Predovic"
     :user/handle (str "lilli" i)
     :user/email  (str "lilli" i "@example.com")}))


(buildset user)
(build [user user])

(defactory article []
  {:article/title "7 Tip-top Things To Try"
   :article/submitter user
   :article/author (with user {:user/roles #{"author"}})})

(set! *print-namespace-maps* false)

(build (with user {:foo "bar"}))
(build (select article [:1 [user]]))

(select* (buildset {:article (article {})})
         [:1 [:article/submitter]])

(defactory carrier []
  {:name "DB"})

(defactory train [{:keys [carrier]
                   :or {carrier "NMBS"}}]
  :primary-key :train-id
  {:carrier carrier
   :fuel-type "electric"})

train

(train {:carrier "DB"})

(build (train {:carrier (carrier)}))
