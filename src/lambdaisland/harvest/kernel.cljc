(ns lambdaisland.harvest.kernel
  "Heart of the Harvest factory logic

  This is strictly a Mechanism namespace: generic, unopinionated, verbose.
  See [[lambdaisland.harvest]] for an interface meant for human consumption."
  (:refer-clojure :exclude [ref])
  (:require [lambdaisland.data-printers :as data-printers]))

(def ^:dynamic *defer-build?* false)

(defrecord DeferredBuild [thunk opts])
(defrecord LVar [id])

(defn defer [thunk opts]
  (->DeferredBuild thunk opts))

(defn factory? [o]
  (= :harvest/factory (:type (meta o))))

(defn unwrap [{:keys [thunk]}]
  (cond
    (var? thunk)
    @thunk
    (factory? thunk)
    (if-let [resolve (:harvest.factory/resolve thunk)]
      (resolve)
      thunk)
    :else
    (thunk)))

(defn deferred-build? [o]
  (instance? DeferredBuild o))

(defn lvar? [o]
  (instance? LVar o))

(declare build)

(defn factory-id [f]
  (cond
    (factory? f)
    (:harvest.factory/id f)
    (deferred-build? f)
    (:harvest.factory/id (unwrap f))))

(defn match1? [p s]
  (or (= s p)
      (= :* s)
      (and (set? s) (some (partial match1? p) s))
      (and (or (factory? s)
               (deferred-build? s))
           (match1? p (factory-id s)))))

(defn path-match? [path selector]
  (let [selector (if (sequential? selector) selector [selector])
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

(defn match-rules [ctx]
  (some #(when (path-match? (:harvest.build/path ctx) (key %))
           (let [v (val %)]
             (if (lvar? v)
               (@(:harvest.build/!lvars ctx) v v)
               v)))
        (:harvest.build/rules ctx)))

(defn factory-template
  [{:harvest.factory/keys [template inherit traits]}
   {:as opts with :with selected-traits :traits}]
  (cond->
      (reduce
       (fn [fact trait]
         (merge fact (get-in traits [trait :with])))
       (if inherit
         (merge (factory-template inherit nil) template)
         template)
       selected-traits)
    with
    (merge with)))

(defn push-path [ctx segment]
  (assert segment)
  (update ctx :harvest.build/path (fnil conj []) segment))

(defn add-linked
  "Add a new entry to the `:harvest.result/linked` map. If there is already an entry
  for the given path then this is a no-op, this allows persistence
  implemenentation to do their own custom linked handling in a hook."
  [result path entity]
  (if (get-in result [:harvest.result/linked path])
    result
    (update result :harvest.result/linked (fnil assoc {}) path entity)))

(defn merge-linked [result linked]
  (update result :harvest.result/linked merge linked))

(defn handle-hook
  ([ctx hook]
   (if-let [h (get ctx hook)]
     (h ctx)
     ctx))
  ([ctx hook value]
   (if-let [h (get ctx hook)]
     (h ctx value)
     value)))

(declare build build-template)

(defn build-factory*
  "Handle building a factory, which means building its template, possibly adjusted
  for traits, and doing some contextual bookkeeping so we can keep track of the
  path within the build process."
  [{:harvest.build/keys [path] :as ctx} factory opts]
  (let [{:harvest.factory/keys [id]} factory
        result (-> ctx
                   (build-template (factory-template factory opts) opts)
                   (assoc :harvest.factory/id id))]
    (if path
      (assoc result :harvest.build/path (:harvest.build/path ctx))
      result)))

(defn build-factory
  [{:as ctx}
   {:as factory, traits :harvest.factory/traits}
   {:as opts, selected-traits :traits}]
  (-> (let [;; for hooks, like input, but guaranteed to be unwrapped
            ctx (assoc ctx :harvest.build/factory factory)
            {:as ctx} (handle-hook ctx :harvest.hooks/before-build-factory)
            {:as   ctx
             path  :harvest.build/path
             value :harvest.result/value} (-> ctx
                                            (build-factory* factory opts))
            ctx (if (and traits selected-traits)
                  (reduce
                   (fn [ctx trait-key]
                     (if-let [hook (:after-build (get traits trait-key))]
                       (hook ctx)
                       ctx))
                   ctx
                   selected-traits)
                  ctx)
            ctx (if-let [hook (:harvest.factory/after-build factory)]
                  (hook ctx)
                  ctx)
            ctx (handle-hook ctx :harvest.hooks/after-build-factory)]
        (cond-> ctx path (add-linked path (:harvest.result/value ctx))))))

(defn build-map-entry
  "Handle a single entry of a map-shaped template (see [[build-template]]),
  handles recursing into building the value, and associng the built value into
  the result."
  [{:as ctx} val-acc k v opts]
  (let [{:as ctx path :harvest.build/path} (push-path ctx k)
        {:harvest.result/keys [value linked] :as result} (build ctx v nil)]
    (-> ctx
        (assoc :harvest.result/value (assoc val-acc (cond->> k
                                                    (factory-id v)
                                                    (handle-hook ctx :harvest.hooks/association-key))
                                          value)
               :harvest.result/linked linked)
        (handle-hook :harvest.hooks/after-build-map-entry))))

(defn build-template
  "Build a value out of a 'template'. This template is basically a data shape that
  describes the shape of the output value. It can be a map (each value gets
  built), a collection like a set or vector (each entry gets built), a
  thunk (the return value gets built), or a plain value (used as is).

  Factories contain templates (as well as an id, hooks, traits, etc). What you
  pass to `harvest/build` is also a template."
  [{:as ctx
    :harvest.build/keys [path]} tmpl opts]
  (cond
    (map? tmpl)
    (reduce-kv
     (fn [acc k v]
       (let [{:harvest.result/keys [value linked]}
             (build-map-entry ctx (:harvest.result/value acc) k v opts)]
         (-> acc
             (assoc :harvest.result/value value)
             (merge-linked linked))))
     (assoc ctx :harvest.result/value (if (record? tmpl)
                                      tmpl
                                      (empty tmpl)))
     tmpl)

    (coll? tmpl)
    (let [results (map-indexed (fn [idx qry]
                                 (build (push-path ctx idx) qry nil))
                               tmpl)]
      (assoc
       ctx
       :harvest.result/value (into (empty tmpl) (map :harvest.result/value) results)
       :harvest.result/linked (transduce (map :harvest.result/linked) merge results)))

    (fn? tmpl)
    (build ctx (tmpl) nil)

    :else
    (assoc ctx :harvest.result/value tmpl)))

(defn extract-hooks [ctx opts hooks]
  (reduce
   (fn [ctx h]
     (if-let [f (get opts h)]
       (assoc ctx (keyword "harvest.hooks" (name h)) f)
       ctx))
   ctx
   hooks))

(defn build
  "Top-level build API, also used when recursing. Handles three cases: factory,
  deferred-build, template. Handles hooks and trait-hooks."
  [ctx input {:as opts
              rules :rules
              selected-traits :traits}]
  (let [opts (dissoc opts :rules)
        lvar-store (:harvest.build/!lvars ctx (volatile! {}))
        ctx (assoc ctx :harvest.build/input input)
        ctx (cond-> ctx
              (not (:harvest.build/!lvars ctx))
              (assoc :harvest.build/!lvars lvar-store)

              rules
              (assoc :harvest.build/rules rules)

              (or (deferred-build? input) (factory? input))
              (push-path (factory-id input))

              :->
              (extract-hooks opts [:after-build
                                   :association-key
                                   :before-build-factory
                                   :after-build-factory
                                   :after-build-map-entry]))
        rule (match-rules ctx)
        input (if (and rule (not (lvar? rule)))
                rule
                input)
        ctx (cond
              (factory? input)
              (build-factory ctx input opts)

              (deferred-build? input)
              (build-factory ctx (unwrap input) (:opts input))

              :else
              (build-template ctx input opts))
        ;; hook passed-in as option
        result (if-let [hook (:harvest.hooks/after-build ctx)]
                 (hook ctx)
                 ctx)]
    (when (lvar? rule)
      (vswap! lvar-store assoc rule (:harvest.result/value result)))
    result))
