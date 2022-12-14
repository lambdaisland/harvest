(ns lambdaisland.harvest
  "Factories for unit tests, devcards, etc."
  (:refer-clojure :exclude [def])
  (:require [lambdaisland.harvest.kernel :as hk]
            #?(:clj [lambdaisland.harvest.macro-util :as macro-util]))
  #?(:cljs (:require-macros [lambdaisland.harvest])))

(declare build-val)

;; Factories don't have to be records, a simple map with the right keys will do,
;; but we like it to have invoke so they are callable as a shorthand for calling
;; build. They should have {:type :harvest/factory} as metadata.
;; - :harvest.factory/id - fully qualified symbol of the factory var
;; - :harvest.factory/template - the template we will build, often a map but can be anything
;; - :harvest.factory/traits - map of traits (name -> map)
(defrecord Factory []
  #?@(:clj
      (clojure.lang.IFn
       (applyTo [this xs] (clojure.lang.AFn/applyToHelper this xs))
       (invoke [this] this)
       (invoke [this opts] (hk/defer this opts))
       (invoke [this k v] (hk/defer this {k v}))
       (invoke [this ka va kb vb] (hk/defer this {ka va kb vb}))
       (invoke [this ka va kb vb kc vc] (hk/defer this {ka va kb vb kc vc}))
       (invoke [this ka va kb vb kc vc kd vd] (hk/defer this {ka va kb vb kc vc kd vd}))
       (invoke [this ka va kb vb kc vc kd vd ke ve] (hk/defer this {ka va kb vb kc vc kd vd ke ve}))
       (invoke [this ka va kb vb kc vc kd vd ke ve kf vf] (hk/defer this {ka va kb vb kc vc kd vd ke ve kf vf}))
       (invoke [this ka va kb vb kc vc kd vd ke ve kf vf kg vg] (hk/defer this {ka va kb vb kc vc kd vd ke ve kf vf kg vg}))
       (invoke [this ka va kb vb kc vc kd vd ke ve kf vf kg vg kh vh] (hk/defer this {ka va kb vb kc vc kd vd ke ve kf vf kg vg kh vh})))
      :cljs
      (cljs.core/IFn
       (-invoke [this] this)
       (-invoke [this opts] (hk/defer this opts))
       (-invoke [this k v] (hk/defer this {k v}))
       (-invoke [this ka va kb vb] (hk/defer this {ka va kb vb}))
       (-invoke [this ka va kb vb kc vc] (hk/defer this {ka va kb vb kc vc}))
       (-invoke [this ka va kb vb kc vc kd vd] (hk/defer this {ka va kb vb kc vc kd vd}))
       (-invoke [this ka va kb vb kc vc kd vd ke ve] (hk/defer this {ka va kb vb kc vc kd vd ke ve}))
       (-invoke [this ka va kb vb kc vc kd vd ke ve kf vf] (hk/defer this {ka va kb vb kc vc kd vd ke ve kf vf}))
       (-invoke [this ka va kb vb kc vc kd vd ke ve kf vf kg vg] (hk/defer this {ka va kb vb kc vc kd vd ke ve kf vf kg vg}))
       (-invoke [this ka va kb vb kc vc kd vd ke ve kf vf kg vg kh vh] (hk/defer this {ka va kb vb kc vc kd vd ke ve kf vf kg vg kh vh})))))

(defn factory
  "Create a factory instance, these are just maps with a `(comp :type meta)` of
  `:harvest/factory`. Will take keyword arguments (`:id`, `:traits`), and one
  non-keyword argument which will become the factory template (can also be
  passed explicitly with a `:template` keyword)."
  [& args]
  (loop [m (with-meta (->Factory) {:type :harvest/factory})
         [x & xs] args]
    (cond
      (nil? x)
      m
      (simple-keyword? x)
      (recur (assoc m (keyword "harvest.factory" (name x)) (first xs))
             (next xs))
      (qualified-keyword? x)
      (recur (assoc m x (first xs))
             (next xs))
      :else
      (recur (assoc m :harvest.factory/template x)
             xs))))

#?(:clj
   (defmacro defactory
     "Factory convenience macro. Takes a name and keyword-value pairs, if a keyword
  is omitted then the value is treated as the factory template. Simple keywords
  are namespaced to `harvest.factory`."
     [fact-name & args]
     `(def ~fact-name
        (binding [hk/*defer-build?* true]
          (factory :id '~(macro-util/qualify-sym &env fact-name)
                   :resolve #(do ~fact-name)
                   ~@args)))))

(defn with [factory opts]
  (hk/defer factory {:with opts}))

(defn with-opts [factory opts]
  (hk/defer factory opts))

(defn unify
  "Use in rules to signify that certain parts of the build tree should be unified,
  i.e. that they should share the same value. It plays a similar role as an lvar
  in logic programming. Invocations without arguments return unique lvars,
  invocations with an id argument are idempotent, allowing unification across
  multiple rules with the same lvar."
  ([]
   (unify (gensym "unify")))
  ([id]
   (hk/->LVar id)))

(defn build
  "Build the given factory or template, returns a result map with
  `:harvest.result/value` and `:harvest.result/linked`."
  ([factory]
   (build factory nil))
  ([factory & {:as opts}]
   (hk/build nil factory opts)))

(defn build-val
  "Build the given factory or template, returns the result value, discarding the
  linked entities."
  ([factory]
   (build-val factory nil))
  ([factory & {:as opts}]
   (if-let [thunk (and hk/*defer-build?* (:harvest.factory/resolve factory))]
     (hk/defer thunk opts)
     (:harvest.result/value (hk/build nil factory opts)))))

(defn all
  "Given a build result, return the built value as well as any linked entities."
  [{:harvest.result/keys [value linked] :as res}]
  (into [value] (map val linked)))

(defn build-all
  "Build the given factory or template. Returns a sequence of all entities that were built."
  ([factory]
   (build-all factory nil))
  ([factory rules]
   (build-all factory rules nil))
  ([factory rules opts]
   (all (hk/build nil factory opts))))

(defn value
  "Given the result map returned by [[build]], retrieve the built value."
  [result]
  (:harvest.result/value result))

(defn sel
  "Given the result map returned by [[build]], return any entities that match the
  given selector."
  [result selector]
  (let [selector (if (vector? selector) selector [selector])]
    (keep #(when (hk/path-match? (key %) selector)
             (val %))
          (:harvest.result/linked result))))

(defn sel1
  "Given the result map returned by [[build]], return the first entity that
  matches the given selector."
  [result selector]
  (first (sel result selector)))

(defn update-result
  "Update the result value in a context map, useful in hooks."
  [ctx f & args]
  (apply update ctx :harvest.result/value f args))
